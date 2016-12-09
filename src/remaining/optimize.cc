#include "optimize.hh"

/*** This file contains all code pertaining to AST optimisation. It currently
     implements a simple optimisation called "constant folding". Most of the
     methods in this file are empty, or just relay optimize calls downward
     in the AST. If a more powerful AST optimization scheme were to be
     implemented, only methods in this file should need to be changed. ***/


ast_optimizer *optimizer = new ast_optimizer();


/* The optimizer's interface method. Starts a recursive optimize call down
   the AST nodes, searching for binary operators with constant children. */
void ast_optimizer::do_optimize(ast_stmt_list *body)
{
    if (body != NULL) {
        body->optimize();
    }
}


/* Returns 1 if an AST expression is a subclass of ast_binaryoperation,
   ie, eligible for constant folding. */
bool ast_optimizer::is_binop(ast_expression *node)
{
    switch (node->tag) {
    case AST_ADD:
    case AST_SUB:
    case AST_OR:
    case AST_AND:
    case AST_MULT:
    case AST_DIVIDE:
    case AST_IDIV:
    case AST_MOD:
        return true;
    default:
        return false;
    }
}

bool ast_optimizer::is_const(ast_expression* expr) {

  if(expr->tag == AST_INTEGER || expr->tag == AST_REAL) {
    return true;
  }
  return false;
}

ast_expression* ast_optimizer::optimize_binop(ast_binaryoperation *node) {

  ast_expression *left = fold_constants(node->left);
  ast_expression *right = fold_constants(node->right);

  if(!(is_const(left) && is_const(right))) {
    return node;
  }

  if(left->type==integer_type && right->type==integer_type) {
    long ll = left->get_ast_integer()->value;
    long rr = right->get_ast_integer()->value;

    switch (node->tag) {
      case AST_ADD:
        return new ast_integer(node->pos, ll + rr);
      case AST_SUB:
        return new ast_integer(node->pos, ll - rr);
      case AST_OR:
        return new ast_integer(node->pos, ll || rr);
      case AST_AND:
        return new ast_integer(node->pos, ll && rr);
      case AST_MULT:
        return new ast_integer(node->pos, ll * rr);
      case AST_IDIV:
        return new ast_integer(node->pos, ll / rr);
      case AST_DIVIDE:
        return new ast_real(node->pos, ll / rr);
      case AST_MOD:
        return new ast_integer(node->pos, ll % rr);
      default:
      return node;
    }

  }  else if(left->type==real_type && right->type==real_type) {
    double ll = left->get_ast_real()->value;
    double rr = right->get_ast_real()->value;

    switch (node->tag) {
      case AST_ADD:
        return new ast_real(node->pos, ll + rr);
      case AST_SUB:
        return new ast_real(node->pos, ll - rr);
      case AST_OR:
        return new ast_real(node->pos, ll || rr);
      case AST_AND:
        return new ast_real(node->pos, ll && rr);
      case AST_MULT:
        return new ast_real(node->pos, ll * rr);
      case AST_DIVIDE:
        return new ast_real(node->pos, ll / rr);
      default:
      return node;
    }
  }
  return node;
}



/* We overload this method for the various ast_node subclasses that can
   appear in the AST. By use of virtual (dynamic) methods, we ensure that
   the correct method is invoked even if the pointers in the AST refer to
   one of the abstract classes such as ast_expression or ast_statement. */
void ast_node::optimize()
{
    fatal("Trying to optimize abstract class ast_node.");
}

void ast_statement::optimize()
{
    fatal("Trying to optimize abstract class ast_statement.");
}

void ast_expression::optimize()
{
    fatal("Trying to optimize abstract class ast_expression.");
}

void ast_lvalue::optimize()
{
    fatal("Trying to optimize abstract class ast_lvalue.");
}

void ast_binaryoperation::optimize()
{
    fatal("Trying to optimize abstract class ast_binaryoperation.");
}

void ast_binaryrelation::optimize()
{
    fatal("Trying to optimize abstract class ast_binaryrelation.");
}



/*** The optimize methods for the concrete AST classes. ***/

/* Optimize a statement list. */
void ast_stmt_list::optimize()
{
    if (preceding != NULL) {
        preceding->optimize();
    }
    if (last_stmt != NULL) {
        last_stmt->optimize();
    }
}


/* Optimize a list of expressions. */
void ast_expr_list::optimize()
{
    /* Your code here */
    if(preceding!=NULL) {
      preceding->optimize();
    }
    if(last_expr!=NULL) {
      last_expr = optimizer->fold_constants(last_expr);
    }
}


/* Optimize an elsif list. */
void ast_elsif_list::optimize()
{
    /* Your code here */
    if(preceding!=NULL) {
      preceding->optimize();
    }
    if(last_elsif!=NULL) {
      last_elsif->optimize();
    }
}


/* An identifier's value can change at run-time, so we can't perform
   constant folding optimization on it unless it is a constant.
   Thus we just do nothing here. It can be treated in the fold_constants()
   method, however. */
void ast_id::optimize()
{
}

void ast_indexed::optimize()
{
    /* Your code here */
}



/* This convenience method is used to apply constant folding to all
   binary operations. It returns either the resulting optimized node or the
   original node if no optimization could be performed. */
ast_expression *ast_optimizer::fold_constants(ast_expression *node)
{
    /* Your code here */
    node->optimize();

    if(is_binop(node)) {
        node = optimize_binop(node->get_ast_binaryoperation());
    }

    return node;
}

/* All the binary operations should already have been detected in their parent
   nodes, so we don't need to do anything at all here. */
void ast_add::optimize()
{
    /* Your code here */
}

void ast_sub::optimize()
{
    /* Your code here */
}

void ast_mult::optimize()
{
    /* Your code here */
}

void ast_divide::optimize()
{
    /* Your code here */
}

void ast_or::optimize()
{
    /* Your code here */
}

void ast_and::optimize()
{
    /* Your code here */
}

void ast_idiv::optimize()
{
    /* Your code here */
}

void ast_mod::optimize()
{
    /* Your code here */
}



/* We can apply constant folding to binary relations as well. */
void ast_equal::optimize()
{
    /* Your code here */
}

void ast_notequal::optimize()
{
    /* Your code here */
}

void ast_lessthan::optimize()
{
    /* Your code here */
}

void ast_greaterthan::optimize()
{
    /* Your code here */
}



/*** The various classes derived from ast_statement. ***/

void ast_procedurecall::optimize()
{
    /* Your code here */
}


void ast_assign::optimize()
{
    /* Your code here */
}


void ast_while::optimize()
{
    /* Your code here */
}


void ast_if::optimize()
{
    /* Your code here */
}


void ast_return::optimize()
{
    /* Your code here */
}


void ast_functioncall::optimize()
{
    /* Your code here */
}

void ast_uminus::optimize()
{
    /* Your code here */
}

void ast_not::optimize()
{
    /* Your code here */
}


void ast_elsif::optimize()
{
    /* Your code here */
}



void ast_integer::optimize()
{
    /* Your code here */
}

void ast_real::optimize()
{
    /* Your code here */
}

/* Note: See the comment in fold_constants() about casts and folding. */
void ast_cast::optimize()
{
    /* Your code here */
}



void ast_procedurehead::optimize()
{
    fatal("Trying to call ast_procedurehead::optimize()");
}


void ast_functionhead::optimize()
{
    fatal("Trying to call ast_functionhead::optimize()");
}
