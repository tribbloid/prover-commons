package ai.acyclic.prover.commons.debug

trait SrcDefinition_Imp0 {
  import SrcDefinition.*

  protected type _FileName = sourcecode.File

  implicit def _inlined(
      implicit
      _fileName: _FileName,
      _line: sourcecode.Line,
      _name: sourcecode.Name,
      _enclosing: sourcecode.Enclosing
  ): Inlined = new Inlined(
    _fileName = _fileName,
    _line = _line,
    _name = _name,
    _enclosing
  )
}
