#ifndef __TIL_AST_BLOCK_NODE_H__
#define __TIL_AST_BLOCK_NODE_H__

#include <cdk/ast/typed_node.h>

namespace til {

  /**
   * Class for describing block nodes.
  */
  class block_node : public cdk::basic_node {
    cdk::sequence_node *_declarations, *_instructions;
    
  public:
    block_node(int line_no, cdk::sequence_node *declarations, cdk::sequence_node *instructions) :
      cdk::basic_node(line_no), _declarations(declarations), _instructions(instructions) {}

    cdk::sequence_node *declarations() { return _declarations; }
    cdk::sequence_node *instructions() { return _instructions; }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_block_node(this, level);
    }
  };
} // til

#endif