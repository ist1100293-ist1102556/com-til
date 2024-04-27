#ifndef __TIL_AST_SIZEOF_OPERATOR_NODE_H__
#define __TIL_AST_SIZEOF_OPERATOR_NODE_H__

namespace til {
    /**
   * Class for describing the node that represents the 'sizeof' operator nodes.
   */
  class sizeof_operator_node: public cdk::unary_operation_node {
    public: 
        sizeof_operator_node(int lineno, cdk::expression_node *expr) : cdk::unary_operation_node(lineno, expr) {
        }

        void accept(basic_ast_visitor *sp, int level) {
            sp->do_sizeof_operator_node(this,level);
        }
  };
} //til

#endif