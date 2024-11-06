package ai.acyclic.prover.commons.meta

trait ITyper extends SymbolViewMixin with TypeViewMixin {

  lazy val rootPackageSymbol: universe.Symbol = {

    val tt = universe.typeOf[ITyper.type]
    val sv = symbolView(tt.typeSymbol)

    sv.Owners.internal.leftOpt.get
  }
}

object ITyper {}
