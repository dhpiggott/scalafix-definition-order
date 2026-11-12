package io.github.dhpiggott.scalafix

import scala.meta.*

import scalafix.v1.*

// What the rule orders, read from the tree alone: the scopes of a file, the
// definitions in each, and the items they are ordered as.
private[scalafix] object Scopes {

  def covers(stat: Stat, offset: Int): Boolean =
    stat.pos.start <= offset && offset < stat.pos.end

  // What an `end` marker closing the definition can name: the definition, or
  // for one without a single name of its own, the keyword that starts it.
  def endMarkerNames(stat: Stat): Set[String] = {
    val keyword = stat match {
      case _: Defn.ExtensionGroup             => Some("extension")
      case _: Defn.Given | _: Defn.GivenAlias => Some("given")
      case _: Defn.Val                        => Some("val")
      case _: Defn.Var                        => Some("var")
      case _                                  => None
    }
    definedNames(stat).map(_.value).toSet ++ keyword
  }

  // An abstract member: a `def`, `val`, `var`, `type` or `given` with no
  // body, or an extension block whose methods all have none.
  def isDeclaration(stat: Stat): Boolean = stat match {
    case _: Decl                   => true
    case defn: Defn.ExtensionGroup =>
      extensionMembers(defn).forall(isDeclaration)
    case _ => false
  }

  // A statement in a scope that defines something, with the names it
  // defines: one for most, several for a pattern `val (a, b) = ...` or an
  // extension block. A statement that defines nothing, such as an import or
  // an expression, isn't a member. Members are grouped into items.
  final case class Member(stat: Stat, names: List[Name])

  // Offsets into the file, end exclusive.
  final case class Span(start: Int, end: Int) {
    def contains(other: Span): Boolean =
      start <= other.start && other.end <= end

    def overlaps(other: Span): Boolean = start < other.end && other.start < end
  }

  // Where a definition can be seen from, widest first. A qualified
  // modifier, such as `private[pkg]`, is the same tier as a bare one.
  sealed abstract class Visibility(val rank: Int)

  object Visibility {
    // Kept widest first, as they rank, rather than by name.
    // scalafix:off DefinitionOrder
    case object Public extends Visibility(0)
    case object Protected extends Visibility(1)
    case object Private extends Visibility(2)
    // scalafix:on DefinitionOrder

    implicit val ordering: Ordering[Visibility] = Ordering.by(_.rank)
  }

  // How whatever the uses leave open is ordered: public, then protected,
  // then private, then by name, then by source text, which tells overloads
  // apart.
  final case class SortKey(
      visibility: Visibility,
      name: String,
      source: String
  )

  object SortKey {
    implicit val ordering: Ordering[SortKey] =
      Ordering.by(key => (key.visibility, key.name, key.source))
  }

  // What the rule moves as one: a definition, a class or trait with its
  // companion, or everything in one kept range.
  final case class Item(
      stats: List[Stat],
      kept: Option[Span],
      key: SortKey,
      isType: Boolean,
      isDeclaration: Boolean,
      isStrict: Boolean,
      symbols: Set[Symbol]
  )

  // An item's index in its scope's `items`. Orders, edges and phases (see
  // `CanonicalOrder.orderOf`) are made of these; most of the other Ints are
  // offsets into the file.
  type ItemId = Int

  // A scope's items, and the order they go in (their ids, top to bottom).
  final case class Ordered(items: Vector[Item], order: List[ItemId])

  // Whether the members are already written in the canonical order.
  def isInOrder(members: List[Member], ordered: Ordered): Boolean = {
    val canonical = ordered.order.flatMap(ordered.items(_).stats)
    canonical.map(_.pos.start) == members.map(_.stat.pos.start)
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

  // Only a class, trait or enum body is a `classBody`, which puts its
  // declarations and fields first: they are its contract and each instance's
  // state. The strict vals of an object, an extension block or a file's top
  // level are constants, placed by their uses like a `def`.
  final case class Scope(stats: List[Stat], classBody: Boolean)

  def membersOf(scope: Scope): List[Member] =
    scope.stats.flatMap { stat =>
      val names = definedNames(stat)
      if (names.isEmpty) None else Some(Member(stat, names))
    }

  // The names a statement defines, or none if it isn't a definition, such as
  // an import or an expression. An anonymous given's name is empty; nothing
  // can refer to it, so it defines nothing here.
  private def definedNames(stat: Stat): List[Name] = {
    val names = stat match {
      case defn: Defn => definitionNames(defn)
      case decl: Decl => declarationNames(decl)
      case _          => Nil
    }
    names.filter(_.value.nonEmpty)
  }

  // Every kind of `Defn` is named, so the only ones the last case can see
  // are scalameta's quasiquote placeholders, which parsed source never holds;
  // they're private to scalameta, so the compiler can't be shown that.
  // A `val` or `var` has patterns (`pats`) on its left rather than a name:
  // `val a, b = 0` has two, and `val (x, y) = pair` one, which binds two
  // names. An enum case is part of its enum, not a definition of its own.
  private def definitionNames(defn: Defn): List[Name] = defn match {
    case defn: Defn.Def            => List(defn.name)
    case defn: Defn.Macro          => List(defn.name)
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
    case _: Defn.EnumCase | _: Defn.RepeatedEnumCase => Nil
    case _                                           => Nil
  }

  // An extension's methods can be abstract, in a trait.
  private def extensionNames(defn: Defn.ExtensionGroup): List[Name] =
    extensionMembers(defn).flatMap {
      case member: Defn.Def => List(member.name)
      case member: Decl.Def => List(member.name)
      case _                => Nil
    }

  // As with `definitionNames`, every kind is named, and the last case is
  // for quasiquote placeholders only.
  private def declarationNames(decl: Decl): List[Name] = decl match {
    case decl: Decl.Def   => List(decl.name)
    case decl: Decl.Val   => decl.pats.flatMap(patternNames)
    case decl: Decl.Var   => decl.pats.flatMap(patternNames)
    case decl: Decl.Type  => List(decl.name)
    case decl: Decl.Given => List(decl.name)
    case _                => Nil
  }

  // `Pat.Var` is the only pattern that binds a name; `collect` finds it
  // inside the others, such as a tuple, `x @ ...` or `x: T`.
  private def patternNames(pat: Pat): List[Name] =
    pat.collect { case Pat.Var(name) => name }

  def nested(stat: Stat): Option[Scope] = {
    val stats = stat match {
      case defn: Defn.Object         => defn.templ.body.stats
      case defn: Defn.Class          => defn.templ.body.stats
      case defn: Defn.Trait          => defn.templ.body.stats
      case defn: Defn.Enum           => defn.templ.body.stats
      case defn: Defn.ExtensionGroup => extensionMembers(defn)
      case _                         => Nil
    }
    Some(Scope(stats, classBody = isClassLike(stat)))
      .filter(_.stats.nonEmpty)
  }

  // `extension (x: Int) def double = x * 2` has the one method as its body.
  // With several methods, indented or in braces, scalameta holds them in a
  // block, which is a `Term` (its type for expressions) like any other.
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
    Scope(members, classBody = false) :: packages.flatMap(scopes)
  }

  def templateOf(stat: Stat): Option[Template] = stat match {
    case defn: Defn.Object => Some(defn.templ)
    case defn: Defn.Class  => Some(defn.templ)
    case defn: Defn.Trait  => Some(defn.templ)
    case defn: Defn.Enum   => Some(defn.templ)
    case _                 => None
  }

  def visibilityOf(stat: Stat): Visibility =
    modsOf(stat)
      .collectFirst {
        case _: Mod.Private   => Visibility.Private
        case _: Mod.Protected => Visibility.Protected
      }
      .getOrElse(Visibility.Public)

  // Every definition with modifiers is a `Stat.WithMods`, so this needs no
  // list of kinds that a new one could be missing from. Only a statement
  // that can't take modifiers, such as an import or an expression, has none.
  private def modsOf(stat: Stat): List[Mod] = stat match {
    case withMods: Stat.WithMods => withMods.mods
    case _                       => Nil
  }
}
