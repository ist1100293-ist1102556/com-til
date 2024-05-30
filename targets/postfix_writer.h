#ifndef __TIL_TARGETS_POSTFIX_WRITER_H__
#define __TIL_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"

#include <sstream>
#include <set>
#include <cdk/emitters/basic_postfix_emitter.h>

namespace til {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer: public basic_ast_visitor {
    cdk::symbol_table<til::symbol> &_symtab;
    cdk::basic_postfix_emitter &_pf;
    int _lbl;
    // Set of all external declarations that will be declared in
    // postfix after generating the rest of the code.
    std::set<std::string> _extern_decls;
    // Variable used when we are evaluating a variable that has
    // an extern name as identifier (will not generate the same code).
    bool _is_extern;
    // Turning it into true will make any declaration be static (not in the
    // frame of the function)
    bool _static_var = false;
    // Stack of function labels to know in which function the node we
    // are visiting is located at
    std::vector<std::string> _function_labels;
    // Stacks of labels used in the stop and next instructions to jump
    // to next iteration or end of the loop.
    std::vector<int> _loop_stop_labels;
    std::vector<int> _loop_next_labels;
    // Flag indicating if we are visiting declarations of the arguments
    // of a function or other types of declarations (used to help
    // compute the offsets)
    bool _processing_args = false;
    // Offset of the next local variable
    int _offset = 0;
    // Flag indicating if we visited a return/next/continue instruction
    // to detect unreachable code
    bool _end_instruction = 0;

  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<til::symbol> &symtab,
                   cdk::basic_postfix_emitter &pf) :
        basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0) {
    }

  public:
    ~postfix_writer() {
      os().flush();
    }

  private:
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return oss.str();
    }

    bool in_function() {
      return _function_labels.size() > 0 && !_static_var;
    }
    void accept_covariant_node(cdk::expression_node * node, std::shared_ptr<cdk::basic_type> type, int lvl);
    void ID_operation(cdk::binary_operation_node *node, int lvl);
    void PID_operation(cdk::binary_operation_node *node, int lvl);

  public:
    void declarate_externs() {
      for (auto decl : _extern_decls) {
        _pf.EXTERN(decl);
      }
    }

  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

  };

} // til

#endif
