#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"
#include "targets/frame_size_calculator.h"
#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated

//---------------------------------------------------------------------------

void til::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}
void til::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}
void til::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (in_function()) {
    _pf.DOUBLE(node->value()); // push an double
  } else {
    _pf.SDOUBLE(node->value());
  }
}
void til::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.INT(0);
  _pf.EQ();
}
void til::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1 = ++_lbl;

  node->left()->accept(this, lvl);
  _pf.INT(0);
  _pf.NE();
  _pf.DUP32();
  _pf.JZ(mklbl(lbl1));
  node->right()->accept(this, lvl);
  _pf.INT(0);
  _pf.NE();
  _pf.AND();
  _pf.LABEL(mklbl(lbl1));
}
void til::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1 = ++_lbl;

  node->left()->accept(this, lvl);
  _pf.INT(0);
  _pf.NE();
  _pf.DUP32();
  _pf.JNZ(mklbl(lbl1));
  node->right()->accept(this, lvl);
  _pf.INT(0);
  _pf.NE();
  _pf.OR();
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (in_function()) {
    _pf.INT(node->value()); // push an integer
  } else {
    _pf.SINT(node->value());
  }
}

void til::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;

  /* generate the string */
  _pf.RODATA(); // strings are DATA readonly
  _pf.ALIGN(); // make sure we are aligned
  _pf.LABEL(mklbl(lbl1 = ++_lbl)); // give the string a name
  _pf.SSTRING(node->value()); // output string characters

  if (in_function()) {
    /* leave the address on the stack */
    _pf.TEXT(_function_labels.back()); // return to the TEXT segment
    _pf.ADDR(mklbl(lbl1)); // the string to be printed
  } else {
    _pf.DATA(); // return to the DATA segment
    _pf.ALIGN(); // make sure we are aligned
    _pf.SADDR(mklbl(lbl1)); // give the string a name
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_unary_minus_node(cdk::unary_minus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  _pf.NEG(); // 2-complement
}

void til::postfix_writer::do_unary_plus_node(cdk::unary_plus_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  
  // If needed, upgrade
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl);

  // If needed, upgrade
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DADD();
  } else {
    _pf.ADD();
  }
}
void til::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  
  // If needed, upgrade
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl);

  // If needed, upgrade
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DSUB();
  } else {
    _pf.SUB();
  }
}

// Generates the common code between all binary operations that accept
// two operands that can be either integers or doubles, and upgrades
// an integer to double if needed.
void til::postfix_writer::ID_operation(cdk::binary_operation_node *node, int lvl) {
  node->left()->accept(this, lvl);
  // If needed, upgrade
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl);
  // If needed, upgrade
  if (node->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT)) {
    _pf.I2D();
  }
}

// Generates the common code between all binary operations that accept
// two operands that can be either integers, doubles or pointers, while
// the result is always an integer (e.g. equals or not equals), and
// upgrades an integer or pointer to double if needed.
void til::postfix_writer::PID_operation(cdk::binary_operation_node *node, int lvl) {
  node->left()->accept(this, lvl);
  if (!node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }

  node->right()->accept(this, lvl);
  if (!node->right()->is_typed(cdk::TYPE_DOUBLE) && node->left()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.I2D();
  }

  if (node->left()->is_typed(cdk::TYPE_DOUBLE) || node->right()->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }
}

void til::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  ID_operation(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DMUL();
  } else {
    _pf.MUL();
  }
}
void til::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  ID_operation(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DDIV();
  } else {
    _pf.DIV();
  }
}
void til::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}
void til::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  ID_operation(node, lvl);
  
  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }

  _pf.LT();
}
void til::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  ID_operation(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }

  _pf.LE();
}
void til::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  ID_operation(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }

  _pf.GE();
}
void til::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  ID_operation(node, lvl);

  if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.DCMP();
    _pf.INT(0);
  }

  _pf.GT();
}
void til::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  PID_operation(node, lvl);
  _pf.NE();
}
void til::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  PID_operation(node, lvl);
  _pf.EQ();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  auto sym = _symtab.find(node->name());

  if (sym == nullptr) {
    throw "undeclared variable " + node->name();
  }

  if (sym->offset() == 0) { // global
    _pf.ADDR(sym->name());
  } else {
    _pf.LOCAL(sym->offset());
  }
}

void til::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl);
  if (node->type()->size() == 4) {
    _pf.LDINT();
  } else if (node->type()->size() == 8) {
    _pf.LDDOUBLE();
  }
}

void til::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->rvalue()->accept(this, lvl); // determine the new value
  
  if (node->rvalue()->type()->size() == 4) {
    _pf.DUP32();
  } else if (node->rvalue()->type()->size() == 8) {
    _pf.DUP64();
  }

  node->lvalue()->accept(this, lvl); // pushes the target address to stack

  // Store the value
  if (node->rvalue()->type()->size() == 4) {
    _pf.STINT();
  } else if (node->rvalue()->type()->size() == 8) {
    _pf.STDOUBLE();
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_function_node(til::function_node * const node, int lvl) {
  // Note that Simple doesn't have functions. Thus, it doesn't need
  // a function node. However, it must start in the main function.
  // The ProgramNode (representing the whole program) doubles as a
  // main function node.

  // generate the main function (RTS mandates that its name be "_main")
  if (node->is_main()) {
    _pf.TEXT("_main");
    _pf.ALIGN();
    _pf.GLOBAL("_main", _pf.FUNC());
    _pf.LABEL("_main");
    _function_labels.push_back("_main");

    til::frame_size_calculator calc(_compiler, _symtab);
    _symtab.push();
    node->declarations()->accept(&calc, lvl);
    node->instructions()->accept(&calc, lvl);
    _symtab.pop();
    _pf.ENTER(calc.size());
    
    _symtab.push();
    
    auto symbol = std::make_shared<til::symbol>(node->type(), "@", 0);
    _symtab.insert("@", symbol);

    _offset = 0;
    _processing_args = false;
    node->declarations()->accept(this, lvl);
    
    _end_instruction = 0;
    for (size_t i = 0; i < node->instructions()->size(); i++) {
      if (_end_instruction) {
        throw "function has a return node in the middle of the code";
      }
      node->instructions()->node(i)->accept(this, lvl);
    }
    auto type = std::dynamic_pointer_cast<cdk::functional_type>(node->type());
    if (!_end_instruction && (type->output(0)->name() != cdk::TYPE_VOID)){
      throw "function has no return node";
    }
    _end_instruction = 0;

    _function_labels.pop_back();
    _symtab.pop();
    // these are just a few library function imports
    _pf.EXTERN("readi");
    _pf.EXTERN("readd");
    _pf.EXTERN("printi");
    _pf.EXTERN("prints");
    _pf.EXTERN("printd");
    _pf.EXTERN("println");
  } else {
    int lbl1 = ++_lbl;
    std::string lbl = mklbl(lbl1);
    _pf.TEXT(lbl);
    _pf.ALIGN();
    _pf.LABEL(lbl);
    _function_labels.push_back(lbl);

    std::vector<int> old_loop_stop_labels = _loop_stop_labels;
    std::vector<int> old_loop_next_labels = _loop_next_labels;
    _loop_stop_labels.clear();
    _loop_next_labels.clear();

    til::frame_size_calculator calc(_compiler, _symtab);
    _symtab.push();
    node->declarations()->accept(&calc, lvl);
    node->instructions()->accept(&calc, lvl);
    _symtab.pop();
    _pf.ENTER(calc.size());

    _symtab.push();

    auto symbol = std::make_shared<til::symbol>(node->type(), "@", 0);
    _symtab.insert("@", symbol);

    int old_offset = _offset;
    _offset = 8;
    _processing_args = true;
    node->arguments()->accept(this, lvl);
    _offset = 0;
    _processing_args = false;
    node->declarations()->accept(this, lvl);

    _end_instruction = 0;
    for (size_t i = 0; i < node->instructions()->size(); i++) {
      if (_end_instruction) {
        throw "function has a return node in the middle of the code";
      }
      node->instructions()->node(i)->accept(this, lvl);
    }
    auto type = std::dynamic_pointer_cast<cdk::functional_type>(node->type());
    if (!_end_instruction) {
      if (type->output(0)->name() != cdk::TYPE_VOID){
        throw "function has no return node";
      } else {
        _pf.LEAVE();
        _pf.RET();
      }
    }

    _end_instruction = 0;

    _offset = old_offset;
    _function_labels.pop_back();
    if (in_function()) {
      _pf.TEXT(_function_labels.back());
      _pf.ALIGN();
      _pf.ADDR(lbl);
    } else {
      _pf.DATA();
      _pf.ALIGN();
      _pf.SADDR(lbl);
    }
    _symtab.pop();

    _loop_stop_labels = old_loop_stop_labels;
    _loop_next_labels = old_loop_next_labels;
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_evaluation_node(til::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // evaluate the expression
  _pf.TRASH(node->argument()->type()->size()); // delete the evaluated value
}

void til::postfix_writer::do_print_node(til::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  for(size_t i = 0; i < node->arguments()->size(); i++){
    auto *arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(i));  
    arg->accept(this, lvl); // determine the value to print
    if (arg->is_typed(cdk::TYPE_INT)) {
      _pf.CALL("printi");
      _pf.TRASH(4); // delete the printed value
    } else if (arg->is_typed(cdk::TYPE_STRING)) {
      _pf.CALL("prints");
      _pf.TRASH(4); // delete the printed value's address
    } else if (arg->is_typed(cdk::TYPE_DOUBLE)) {
      _pf.CALL("printd");
      _pf.TRASH(8); // delete the printed value
    } else {
      std::cerr << "ERROR: CANNOT HAPPEN!" << std::endl;
      exit(1);
    }
  }

  if (node->newline()) {
    _pf.CALL("println"); // print a newline
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_read_node(til::read_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (node->is_typed(cdk::TYPE_INT)) {
    _pf.CALL("readi");
    _pf.LDFVAL32();
  } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.CALL("readd");
    _pf.LDFVAL64();
  }
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_loop_node(til::loop_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl_cond, lbl_end;
  _loop_next_labels.push_back(lbl_cond = ++_lbl);
  _loop_stop_labels.push_back(lbl_end = ++_lbl);
  _pf.LABEL(mklbl(lbl_cond));
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl_end));
  node->instruction()->accept(this, lvl);
  _end_instruction = 0;
  _pf.JMP(mklbl(lbl_cond));
  _pf.LABEL(mklbl(lbl_end));
  _loop_next_labels.pop_back();
  _loop_stop_labels.pop_back();
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_if_node(til::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _end_instruction = 0;
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void til::postfix_writer::do_if_else_node(til::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  _end_instruction = 0;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _end_instruction = 0;
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 2);
  _end_instruction = 0;
  _pf.LABEL(mklbl(lbl2));
}

//---------------------------------------------------------------------------
void til::postfix_writer::do_nullptr_node(til::nullptr_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (in_function()) {
    _pf.INT(0); // push an integer
  } else {
    _pf.SINT(0);
  }
}

//---------------------------------------------------------------------------
void til::postfix_writer::do_stop_node(til::stop_node * const node, int lvl) {
  auto level = static_cast<size_t>(node->level());

  if (level == 0 || level > _loop_stop_labels.size()) {
    throw "invalid loop to stop";
  }

  auto label = _loop_stop_labels.at(_loop_stop_labels.size() - level);
  _pf.JMP(mklbl(label));
  _end_instruction = 1;
}

//---------------------------------------------------------------------------
void til::postfix_writer::do_next_node(til::next_node * const node, int lvl) {
  auto level = static_cast<size_t>(node->level());

  if (level == 0 || level > _loop_next_labels.size()) {
    throw "invalid loop to go next";
  }

  auto label = _loop_next_labels.at(_loop_next_labels.size() - level);
  _pf.JMP(mklbl(label));
  _end_instruction = 1;
}

//---------------------------------------------------------------------------
void til::postfix_writer::do_return_node(til::return_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (node->value() != nullptr) {
    node->value()->accept(this, lvl);

    if (node->value()->type()->size() == 4) {
      _pf.STFVAL32();
    } else if (node->value()->type()->size() == 8) {
      _pf.STFVAL64();
    }
  }

  _pf.LEAVE();
  _pf.RET();
  _end_instruction = 1;
}

//---------------------------------------------------------------------------
void til::postfix_writer::do_declaration_node(til::declaration_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (in_function()) {
    if (_processing_args) {
      _symtab.find(node->identifier())->offset(_offset);
      _offset += node->type()->size();
    } else {
      _offset -= node->type()->size();
      _symtab.find(node->identifier())->offset(_offset);
    }
  }

  if (node->qualifier() == 2 || node->qualifier() == 3) {
      _pf.EXTERN(node->identifier());
      return;
    }
  if (in_function()) {
    if (node->initial() != nullptr) {
      node->initial()->accept(this, lvl);
      _pf.LOCAL(_symtab.find(node->identifier())->offset());
      if (node->type()->size() == 4) {
        _pf.STINT();
      } else if (node->type()->size() == 8) {
        _pf.STDOUBLE();
      }
    }
  } else {
    if (node->initial() != nullptr) {
      _pf.DATA();
      _pf.ALIGN();
      // PUBLIC
      if (node->qualifier() == 1) {
        _pf.GLOBAL(node->identifier(), _pf.OBJ());
      }
      _pf.LABEL(node->identifier());
      node->initial()->accept(this, lvl);
    } else {
      _pf.BSS();
      _pf.ALIGN();
      // PUBLIC
      if (node->qualifier() == 1) {
        _pf.GLOBAL(node->identifier(), _pf.OBJ());
      }
      _pf.LABEL(node->identifier());
      _pf.SALLOC(node->type()->size());
    }
  }
}

//---------------------------------------------------------------------------
void til::postfix_writer::do_block_node(til::block_node * const node, int lvl) {
  _symtab.push();
  node->declarations()->accept(this, lvl);
  _end_instruction = 0;
  for (size_t i = 0; i < node->instructions()->size(); i++) {
    if (_end_instruction) {
      throw "block has a return node in the middle of the code";
    }
    node->instructions()->node(i)->accept(this, lvl);
  }
  _end_instruction = 0;
  _symtab.pop();
}

//---------------------------------------------------------------------------
void til::postfix_writer::do_function_call_node(til::function_call_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int args_size = 0;
  for (int i = node->args()->size()-1; i >= 0; i--) {
    auto arg = dynamic_cast<cdk::expression_node*>(node->args()->node(i));
    arg->accept(this, lvl);
    args_size += arg->type()->size();
  }

  if (node->function_pointer() == nullptr) {
    _pf.CALL(_function_labels.back());
  } else {
    node->function_pointer()->accept(this, lvl);
    _pf.BRANCH();
  }
  _pf.TRASH(args_size);
  _pf.ALIGN();

  if (node->type()->size() == 4) {
    _pf.LDFVAL32();
  } else if (node->type()->size() == 8) {
    _pf.LDFVAL64();
  }
}


//---------------------------------------------------------------------------
void til::postfix_writer::do_objects_operator_node(til::objects_operator_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  if (!node->is_typed(cdk::TYPE_POINTER)) {
    throw std::string("trying to assign objects operator to a non pointer variable");
  }

  auto type = cdk::reference_type::cast(node->type());

  node->argument()->accept(this, lvl);
  _pf.INT(type->referenced()->size());
  _pf.MUL();
  _pf.ALLOC();
  _pf.SP();
}

//---------------------------------------------------------------------------
void til::postfix_writer::do_sizeof_operator_node(til::sizeof_operator_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  _pf.INT(node->argument()->type()->size());
}

//---------------------------------------------------------------------------
void til::postfix_writer::do_referencing_operator_node(til::referencing_operator_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  
  node->lval()->accept(this, lvl);
}

//---------------------------------------------------------------------------
void til::postfix_writer::do_pointer_indexing_node(til::pointer_indexing_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->pointer()->accept(this, lvl);
  node->index()->accept(this, lvl);
  _pf.INT(node->type()->size());
  _pf.MUL();
  _pf.ADD();
}