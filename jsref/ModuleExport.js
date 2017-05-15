// Export the JsInterpreter ML module as a NodeJS module
try {
  module.exports = {
    CalcInterpreter: CalcInterpreter,
    JsInterpreter: JsInterpreter,
    JsInterpreterMonads: JsInterpreterMonads,
    JsInterpreterUtils: JsInterpreterUtils,
    JsCommonAux: JsCommonAux,
    HeapStr: HeapStr
  }
} catch (e) {}
