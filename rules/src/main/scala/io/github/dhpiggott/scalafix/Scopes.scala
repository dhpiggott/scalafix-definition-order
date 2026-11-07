package io.github.dhpiggott.scalafix

import scala.meta.*

import scalafix.v1.*

// What the rule orders, read from the tree alone: the scopes of a file, the
// definitions in each, and the items they are ordered as.
private[scalafix] object Scopes {

  def covers(stat: Stat, offset: Int): Boolean =
    stat.pos.start <= offset && offset < stat.pos.end

  final case class Member(stat: Stat, names: List[Name])

  // Offsets into the file, end exclusive.
  final case class Span(start: Int, end: Int) {
    def contains(other: Span): Boolean =
      start <= other.start && other.end <= end

    def overlaps(other: Span): Boolean = start < other.end && other.start < end
  }

  // How whatever the uses leave open is ordered: public before private, then
  // by name, then by source text, which tells overloads apart.
  final case class SortKey(isPrivate: Boolean, name: String, source: String)

  object SortKey {
    implicit val ordering: Ordering[SortKey] =
      Ordering.by(key => (key.isPrivate, key.name, key.source))
  }

  // What the rule moves as one: a definition, a class or trait with its
  // companion, or everything in one kept range.
  final case class Item(
      stats: List[Stat],
      kept: Option[Span],
      key: SortKey,
      isType: Boolean,
      isStrict: Boolean,
      symbols: Set[Symbol]
  )

  // An item's index in its scope's `items`. Orders, edges and phases are made
  // of these; most of the other Ints are offsets into the file.
  type ItemId = Int

  // A scope's items, and the order they go in (their ids, top to bottom).
  final case class Ordered(items: Vector[Item], order: List[ItemId])

  // Whether the members are already written in the canonical order.
  def isInOrder(members: List[Member], ordered: Ordered): Boolean = {
    val canonical = ordered.order.flatMap(ordered.items(_).stats)
    canonical.map(_.pos.start) == members.map(_.stat.pos.start)
  }

  def isPrivate(stat: Stat): Boolean =
    modsOf(stat).exists {
      case _: Mod.Private | _: Mod.Protected => true
      case _                                 => false
    }

  private def modsOf(stat: Stat): List[Mod] = stat match {
    case defn: Defn.Def        => defn.mods
    case defn: Defn.Val        => defn.mods
    case defn: Defn.Var        => defn.mods
    case defn: Defn.Type       => defn.mods
    case defn: Defn.Class      => defn.mods
    case defn: Defn.Trait      => defn.mods
    case defn: Defn.Object     => defn.mods
    case defn: Defn.Enum       => defn.mods
    case defn: Defn.Given      => defn.mods
    case defn: Defn.GivenAlias => defn.mods
    case decl: Decl.Def        => decl.mods
    case decl: Decl.Val        => decl.mods
    case decl: Decl.Type       => decl.mods
    case _                     => Nil
  }

  def isStrict(stat: Stat): Boolean = stat match {
    case defn: Defn.Val => !defn.mods.exists(_.isInstanceOf[Mod.Lazy])
    case _: Defn.Var    => true
    case _              => false
  }

  def isType(stat: Stat): Boolean = stat match {
    case _: Defn.Type | _: Decl.Type => true
    case _                           => isClassLike(stat)
  }

  // Only a class, trait or enum body is `fieldsFirst`.
  final case class Scope(stats: List[Stat], fieldsFirst: Boolean)

  def membersOf(scope: Scope): List[Member] =
    scope.stats.flatMap(stat => definedNames(stat).map(Member(stat, _)))

  private def definedNames(stat: Stat): Option[List[Name]] = {
    val names = stat match {
      case defn: Defn.Def            => List(defn.name)
      case defn: Defn.Val            => defn.pats.flatMap(patternNames)
      case defn: Defn.Var            => defn.pats.flatMap(patternNames)
      case defn: Defn.Type           => List(defn.name)
      case defn: Defn.Class          => List(defn.name)
      case defn: Defn.Trait          => List(defn.name)
      case defn: Defn.Object         => List(defn.name)
      case defn: Defn.Enum           => List(defn.name)
      case defn: Defn.Given          => List(defn.name)
      case defn: Defn.GivenAlias     => List(defn.name)
      case defn: Defn.ExtensionGroup => extensionNames(defn)
      case decl: Decl.Def            => List(decl.name)
      case decl: Decl.Val            => decl.pats.flatMap(patternNames)
      case decl: Decl.Type           => List(decl.name)
      case _                         => Nil
    }
    Some(names.filter(_.value.nonEmpty)).filter(_.nonEmpty)
  }

  private def patternNames(pat: Pat): List[Name] =
    pat.collect { case Pat.Var(name) => name }

  private def extensionNames(defn: Defn.ExtensionGroup): List[Name] =
    extensionMembers(defn).flatMap {
      case member: Defn.Def => List(member.name)
      case member: Decl.Def => List(member.name)
      case _                => Nil
    }

  def nested(stat: Stat): Option[Scope] = {
    val stats = stat match {
      case defn: Defn.Object         => defn.templ.body.stats
      case defn: Defn.Class          => defn.templ.body.stats
      case defn: Defn.Trait          => defn.templ.body.stats
      case defn: Defn.Enum           => defn.templ.body.stats
      case defn: Defn.ExtensionGroup => extensionMembers(defn)
      case _                         => Nil
    }
    Some(Scope(stats, fieldsFirst = isClassLike(stat)))
      .filter(_.stats.nonEmpty)
  }

  private def extensionMembers(defn: Defn.ExtensionGroup): List[Stat] =
    defn.body match {
      case block: Term.Block => block.stats
      case stat              => List(stat)
    }

  // A class, trait or enum: what has instances, fields, and a companion in
  // SemanticDB even when none is written out.
  def isClassLike(stat: Stat): Boolean = stat match {
    case _: Defn.Class | _: Defn.Trait | _: Defn.Enum => true
    case _                                            => false
  }

  def scopes(tree: Tree): List[Scope] = tree match {
    case source: Source => scopesIn(source.stats)
    case pkg: Pkg       => scopesIn(pkg.body.stats)
    case _              => Nil
  }

  private def scopesIn(stats: List[Stat]): List[Scope] = {
    val (packages, members) = stats.partition(_.isInstanceOf[Pkg])
    Scope(members, fieldsFirst = false) :: packages.flatMap(scopes)
  }
}
