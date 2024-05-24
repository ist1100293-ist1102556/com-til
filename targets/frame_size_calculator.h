#ifndef __TIL_TARGETS_FRAME_SIZE_CALCULATOR_H__
#define __TIL_TARGETS_FRAME_SIZE_CALCULATOR_H__

#include "targets/basic_ast_visitor.h"

#include <sstream>
#include <cdk/emitters/basic_postfix_emitter.h>

namespace til {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class frame_size_calculator: public basic_ast_visitor {
    cdk::symbol_table<til::symbol> &_symtab;
    ssize_t _size = 0;

  public:
    frame_size_calculator(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<til::symbol> &symtab) :
        basic_ast_visitor(compiler), _symtab(symtab), _size(0) {
    }

  public:
    ~frame_size_calculator() {
      os().flush();
    }
  public:
    inline ssize_t size() {
        return _size;
    }
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

  };

} // til

#endif
