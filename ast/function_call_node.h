#ifndef __TIL_AST_FUNCTION_CALL_NODE_H__
#define __TIL_AST_FUNCTION_CALL_NODE_H__

#include <cdk/ast/expression_node.h>

namespace til {

  /**
   * Class for describing function call nodes.
  */
  class function_call_node : public cdk::expression_node {
    cdk::expression_node *_function_pointer;
    cdk::sequence_node *_args;
    
  public:
    function_call_node(int line_no, cdk::expression_node *function_pointer, cdk::sequence_node *args) :
      cdk::expression_node(line_no), _function_pointer(function_pointer), _args(args) {}

    cdk::expression_node *function_pointer() { return _function_pointer; }
    cdk::sequence_node *args() { return _args; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_call_node(this, level);
    }
  };
} // til

#endif