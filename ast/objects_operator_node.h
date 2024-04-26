#ifndef __TIL_AST_OBJECTS_OPERATOR_NODE_H__
#define __TIL_AST_OBJECTS_OPERATOR_NODE_H__

namespace til {
    /**
   * Class for describing the node that represents the 'object' operator nodes.
   */
  class objects_operator_node: public cdk::unary_operation_node {
    public: 
        objects_operator_node(int lineno, cdk::expression_node *num) : cdk::unary_operation_node(lineno, num) {
        }

        void accept(basic_ast_visitor *sp, int level) {
            sp->do_objects_operator_node(this,level);
        }
  };
} //til

#endif