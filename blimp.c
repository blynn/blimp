#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <readline/readline.h>
#include <readline/history.h>
#include <unistd.h>
#include <gmp.h>
#include "blt.h"

#define CHECK(_x_) if (_x_); else die("FAIL %s:%d", __FILE__, __LINE__)

#define NORETURN __attribute__((__noreturn__))

void die(const char *err, ...) NORETURN __attribute__((format (printf, 1, 2)));
void die(const char *err, ...) {
  va_list params;

  va_start(params, err);
  vfprintf(stderr, err, params);
  fputc('\n', stderr);
  va_end(params);
  exit(1);
}

enum { T_FUN = 0, T_CONS, T_MPZ, T_SYM, T_ERR, };

struct lambda_s;
typedef struct lambda_s *lambda_t;

struct sym_list_s;
typedef struct sym_list_s *sym_list_ptr;

struct node_s;
typedef struct node_s *node_t;
struct node_s {
  int type;
  node_t car, cdr;
  mpz_t z;
  char *s;
  node_t (*fun)(node_t, sym_list_ptr);
  lambda_t lambda;
};

struct lambda_s {
  char **arg;
  int argn;
  node_t body;
  sym_list_ptr syms;
};

lambda_t lambda_new() {
  lambda_t r = malloc(sizeof(*r));
  r->arg = 0;
  r->argn = 0;
  r->body = 0;
  return r;
}

void lambda_add_arg(lambda_t lambda, char *s) {
  lambda->arg = realloc(lambda->arg, (lambda->argn + 1) * sizeof(char *));
  lambda->arg[lambda->argn++] = strdup(s);
}

void show(node_t node) {
  if (!node) {
    fputs("nil", stdout);
    return;
  }
  switch (node->type) {
  case T_CONS:
    putchar('(');
    show(node->car);
    printf(" . ");
    show(node->cdr);
    putchar(')');
    break;
  case T_MPZ:
    mpz_out_str(stdout, 0, node->z);
    break;
  case T_SYM:
    fputs(node->s, stdout);
    break;
  case T_FUN:
    fputs("[function]", stdout);
    break;
  case T_ERR:
    printf("ERROR: %s\n", node->s);
    break;
  default:
    die("unhandled node type");
    break;
  }
}

BLT *special;

node_t node_new_mpz() {
  node_t r = malloc(sizeof(*r));
  r->type = T_MPZ;
  mpz_init(r->z);
  return r;
}

BLT *allsyms;

node_t node_new_sym(char *s) {
  BLT_IT *it = blt_set(allsyms, s);
  if (!it->data) {
    node_t r = malloc(sizeof(*r));
    r->type = T_SYM;
    r->s = strdup(s);
    it->data = r;
  }
  return it->data;
}

node_t node_err(char *s) {
  node_t r = malloc(sizeof(*r));
  r->type = T_ERR;
  r->s = strdup(s);
  return r;
}

node_t sym_quote, sym_if, sym_cond, sym_t, sym_nil, sym_lambda, sym_let,
    sym_defun, sym_progn;

struct sym_list_s {
  sym_list_ptr next;
  BLT *sym;
};
typedef struct sym_list_s sym_list_t[1];

#define EVAL_CHECK(_x_) ({  \
  node_t r = eval(_x_, syms); \
  if (r && r->type == T_ERR) return r; \
  r; \
})
#define CAR(_x_) ({ \
  if (!_x_ || _x_->type != T_CONS) return node_err("CAR: expected cons"); \
  _x_->car; \
})
#define CDR(_x_) ({ \
  if (!_x_ || _x_->type != T_CONS) return node_err("CDR: expected cons"); \
  _x_->cdr; \
})
node_t eval(node_t node, sym_list_t syms);

node_t run(node_t fun, node_t arg, sym_list_ptr syms) {
  if (fun->fun) {
    return fun->fun(arg, syms);
  }
  sym_list_t lsym;
  lsym->sym = blt_new();
  lsym->next = fun->lambda->syms;
  sym_list_ptr orig = syms;
  int n = 0;
  while (arg) {
    if (n >= fun->lambda->argn) return node_err("too many lambda args");
    CHECK(arg->type == T_CONS);
    blt_put(lsym->sym, fun->lambda->arg[n], arg->car);
    arg = arg->cdr;
    n++;
  }
  if (n < fun->lambda->argn) return node_err("too few lambda args");
  syms = lsym;
  node_t r = EVAL_CHECK(fun->lambda->body);
  syms = orig;
  return r;
}

node_t eval(node_t node, sym_list_t syms) {
  if (!node) return 0;
  switch (node->type) {
  case T_CONS:
    if (node->car == sym_quote) return CAR(CDR(node));
    if (node->car == sym_if) {
      node = CDR(node);
      node_t cond = CAR(node);
      node = CDR(node);
      node_t ontrue = CAR(node);
      node = CDR(node);
      node_t onfalse = CAR(node);
      return EVAL_CHECK(cond) ? EVAL_CHECK(ontrue) : EVAL_CHECK(onfalse);
    }
    if (node->car == sym_cond) {
      while ((node = node->cdr)) {
        node_t x = CAR(node);
        node_t r = EVAL_CHECK(CAR(x));
        if (r) {
          while ((x = CDR(x))) r = EVAL_CHECK(x->car);
          return r;
        }
      }
      return 0;
    }
    if (node->car == sym_lambda) {
      lambda_t lambda = lambda_new();
      node = CDR(node);
      node_t vars = CAR(node);
      while (vars) {
        node_t var = CAR(vars);
        if (var->type != T_SYM) return node_err("expected symbol");
        lambda_add_arg(lambda, var->s);
        vars = CDR(vars);
      }
      node = CDR(node);
      lambda->syms = syms;
      lambda->body = node->car;
      if (node->cdr) return node_err("too many args");
      node_t r = malloc(sizeof(*r));
      r->type = T_FUN;
      r->fun = 0;
      r->lambda = lambda;
      return r;
    }
    if (node->car == sym_defun) {
      lambda_t lambda = lambda_new();
      node = CDR(node);
      node_t name = CAR(node);
      if (name->type != T_SYM) return node_err("expected symbol");
      node = CDR(node);
      node_t vars = CAR(node);
      while (vars) {
        node_t var = CAR(vars);
        if (var->type != T_SYM) return node_err("expected symbol");
        lambda_add_arg(lambda, var->s);
        vars = CDR(vars);
      }
      node = CDR(node);
      lambda->syms = syms;
      lambda->body = node->car;
      if (node->cdr) return node_err("too many args");
      node_t r = malloc(sizeof(*r));
      r->type = T_FUN;
      r->fun = 0;
      r->lambda = lambda;
      blt_put(special, name->s, r);
      return r;
    }
    if (node->car == sym_progn) {
      node_t r = 0;
      while ((node = CDR(node))) r = EVAL_CHECK(node->car);
      return r;
    }
    if (node->car == sym_let) {
      sym_list_ptr lsym = malloc(sizeof(*lsym));
      lsym->next = syms;
      lsym->sym = blt_new();
      node = CDR(node);
      node_t vars = CAR(node);
      while (vars) {
        node_t varinit = CAR(vars);
        node_t var = CAR(varinit);
        if (var->type != T_SYM) return node_err("expected symbol");
        if (CDR(CDR(varinit))) return node_err("malformed let binding");
        blt_put(lsym->sym, var->s, EVAL_CHECK(CAR(CDR(varinit))));
        vars = CDR(vars);
      }
      syms = lsym;
      node_t r = 0;
      while ((node = CDR(node))) r = EVAL_CHECK(CAR(node));
      syms = syms->next;
      return r;
    }

    {
      node_t fun = EVAL_CHECK(node->car);
      if (fun->type != T_FUN) return node_err("expected function");
      node_t arg = 0, *p = &arg;
      for (;;) {
        node = node->cdr;
        if (!node) break;
        if (node->type != T_CONS) return node_err("expected list");
        node_t c = malloc(sizeof(*c));
        c->type = T_CONS;
        c->car = EVAL_CHECK(node->car);
        c->cdr = 0;
        *p = c;
        p = &c->cdr;
      }
      return run(fun, arg, syms);
    }
  case T_MPZ: {
    node_t r = node_new_mpz();
    mpz_set(r->z, node->z);
    return r;
  } break;
  case T_SYM:
    if (node == sym_nil) return 0;
    if (node == sym_t) return node;
    for (sym_list_ptr p = syms; p; p = p->next) {
      BLT_IT *it = blt_get(p->sym, node->s);
      if (it) return it->data;
    }
    return node_err("undefined symbol");
  case T_FUN:
    return node;
  }
  die("TODO");
}

node_t node_new_fun(node_t (*fun)(node_t, sym_list_t)) {
  node_t r = malloc(sizeof(*r));
  r->type = T_FUN;
  r->fun = fun;
  return r;
}

int main() {
  special = blt_new();
  blt_put(special, "<", node_new_fun(({node_t _(node_t arg, sym_list_ptr _) {
    if (!arg) return node_err("expected one argument");
    if (arg->car->type != T_MPZ) return node_err("expected int");
    mpz_ptr z = arg->car->z;
    for (;;) {
      arg = arg->cdr;
      if (!arg) return sym_t;
      if (arg->car->type != T_MPZ) return node_err("expected int");
      if (mpz_cmp(z, arg->car->z) >= 0) return 0;
      z = arg->car->z;
    }
  }_;})));
  blt_put(special, "+", node_new_fun(({node_t _(node_t arg, sym_list_ptr _) {
    node_t r = node_new_mpz();
    while (arg) {
      if (arg->car->type != T_MPZ) return node_err("expected int");
      mpz_add(r->z, r->z, arg->car->z);
      arg = arg->cdr;
    }
    return r;
  }_;})));
  blt_put(special, "funcall", node_new_fun(({node_t _(node_t arg, sym_list_ptr syms) {
    if (!arg || arg->type != T_CONS) return node_err("expected cons");
    if (arg->car->type != T_FUN) return node_err("expected function");
    return run(arg->car, arg->cdr, syms);
  }_;})));
  blt_put(special, "null", node_new_fun(({node_t _(node_t arg, sym_list_ptr _) {
    if (!arg) return node_err("expected one argument");
    if (arg->cdr != 0) return node_err("expected only one argument");
    if (arg->car) return 0;
    return sym_t;
  }_;})));
  blt_put(special, "atom", node_new_fun(({node_t _(node_t arg, sym_list_ptr _) {
    if (!arg) return node_err("expected one argument");
    if (arg->cdr != 0) return node_err("expected only one argument");
    if (arg->car->type == T_CONS) return 0;
    return sym_t;
  }_;})));
  blt_put(special, "eq", node_new_fun(({node_t _(node_t arg, sym_list_ptr _) {
    if (!arg || !arg->cdr || arg->cdr->cdr) return node_err("expected two arguments");
    node_t x = arg->car, y = arg->cdr->car;
    if (!x || !y) return x == y ? sym_t : 0;
    if (x->type == T_CONS || x->type != y->type) return 0;
    switch (x->type) {
      case T_FUN:
        if (x->fun) return x->fun == y->fun ? sym_t : 0;
        return x->lambda == y->lambda ? sym_t : 0;
      case T_MPZ:
        return !mpz_cmp(x->z, y->z) ? sym_t : 0;
      case T_SYM:
        return !strcmp(x->s, y->s) ?sym_t : 0;
    }
    return node_err("unhandled case");
  }_;})));
  blt_put(special, "car", node_new_fun(({node_t _(node_t arg, sym_list_ptr _) {
    if (!arg || arg->type != T_CONS) return node_err("expected cons");
    if (arg->cdr != 0) return node_err("expected only one argument");
    if (arg->car->type != T_CONS) return node_err("expected cons");
    return arg->car->car;
  }_;})));
  blt_put(special, "cdr", node_new_fun(({node_t _(node_t arg, sym_list_ptr _) {
    if (!arg || arg->type != T_CONS) return node_err("expected cons");
    if (arg->cdr != 0) return node_err("expected only one argument");
    if (arg->car->type != T_CONS) return node_err("expected cons");
    return arg->car->cdr;
  }_;})));
  blt_put(special, "cons", node_new_fun(({node_t _(node_t arg, sym_list_ptr _) {
    if (!arg || arg->type != T_CONS) return node_err("expected cons");
    if (arg->cdr == 0) return node_err("expected two arguments (got 1)");
    if (arg->cdr->cdr != 0) return node_err("expected only two arguments");
    node_t r = malloc(sizeof(*r));
    r->type = T_CONS;
    r->car = arg->car;
    r->cdr = arg->cdr->car;
    return r;
  }_;})));
  blt_put(special, "set", node_new_fun(({node_t _(node_t arg, sym_list_ptr syms) {
    if (!arg || arg->type != T_CONS) return node_err("expected cons");
    if (arg->cdr == 0) return node_err("expected two arguments (got 1)");
    if (arg->cdr->cdr != 0) return node_err("expected only two arguments");
    if (arg->car->type != T_SYM) return node_err("expected symbol");
    for (sym_list_ptr p = syms; p; p = p->next) {
      BLT_IT *it = blt_get(p->sym, arg->car->s);
      if (it) {
        it->data = arg->cdr->car;
        return it->data;
      }
    }
    //fprintf(stderr, "[warning: undefined var]\n");
    blt_put(special, arg->car->s, arg->cdr->car);
    return arg->cdr->car;
  }_;})));

  mpz_t ztmp;
  mpz_init(ztmp);

  char *line = malloc(1);
  *line = 0;
  char *cursor = line;
  node_t rparen = malloc(sizeof(*rparen));

  void *prompt = "";

  char *(*liner)() = ({char*_() {
    char *r = readline(prompt);
    if (r && *r) add_history(r);
    return r;
  }_;});
  if (!isatty(STDIN_FILENO)) liner = ({char*_(){
    char *r = 0;
    size_t n;
    if (-1 == getline(&r, &n, stdin)) {
      free(r);
      r = 0;
    } else {
      char *c = r + strlen(r) - 1;
      if (*c == '\n') *c = 0;
    }
    return r;
  }_;});

  allsyms = blt_new();
  sym_quote  = node_new_sym("quote");
  sym_if     = node_new_sym("if");
  sym_cond   = node_new_sym("cond");
  sym_nil    = node_new_sym("nil");
  sym_t      = node_new_sym("t");
  sym_lambda = node_new_sym("lambda");
  sym_let    = node_new_sym("let");
  sym_defun  = node_new_sym("defun");
  sym_progn  = node_new_sym("progn");
  node_t parse() {
    for (;;) {
      for(;;) {
        while (*cursor == ' ') cursor++;
        if (*cursor) break;
        free(line);
        line = liner();
        prompt = "";
        if (!line) exit(0);
        cursor = line;
      }
      char *start = cursor;
      if (!strchr("(') ", *cursor++)) {
        while (*cursor && !strchr("(') ", *cursor)) cursor++;
      }

      char *word = strndup(start, cursor - start);
      if (*word == '(') {
        node_t r = 0;
        node_t *p = &r;
        for(;;) {
          node_t item = parse();
          if (rparen == item) return r;
          node_t c = malloc(sizeof(*c));
          c->type = T_CONS;
          c->car = item;
          c->cdr = 0;
          *p = c;
          p = &c->cdr;
        }
      } else if (*word == ')') {
        return rparen;
      } else {
        if (*word == '\'') {
          node_t r = malloc(sizeof(*r));
          r->type = T_CONS;
          r->car = sym_quote;
          r->cdr = malloc(sizeof(*r));
          r->cdr->type = T_CONS;
          r->cdr->car = parse();
          r->cdr->cdr = 0;
          return r;
        } else if (!mpz_set_str(ztmp, word, 0)) {
          node_t r = malloc(sizeof(*r));
          r->type = T_MPZ;
          mpz_init_set(r->z, ztmp);
          return r;
        } else {
          return node_new_sym(word);
        }
      }
      free(word);
    }
  }

  for (;;) {
    prompt = "* ";
    node_t node = parse();
    sym_list_t syms;
    syms->next = 0;
    syms->sym = special;
    show(eval(node, syms));
    putchar('\n');
  }
  mpz_clear(ztmp);
  return 0;
}
