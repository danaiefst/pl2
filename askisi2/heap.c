#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define OTHER 0
#define POINTER 1

int HEAP_SIZE = pow(2, 24);

typedef struct node_st {
  long int data;
  int type;
} node_st;

typedef struct stack_t {
  int head;
  node_st* nodes;
  int size;
} stack;

typedef struct node_hp {
  long int fields[2];
  int field_types[2];
  int is_marked;
  int done;
} node_hp;

stack* init_stack(int size) {
  stack* vm_stack = (stack*)malloc(sizeof(stack));
  vm_stack->head = 0;
  vm_stack->size = size;
  vm_stack->nodes = (node_st*)malloc(size*sizeof(node_st));
  return vm_stack;
}

int full(stack* vm_stack){
  if (vm_stack->head == vm_stack->size) {
    return 1;
  }
  return 0;
}

int empty(stack* vm_stack) {
  if (vm_stack->head == 0) {
    return 1;
  }
  return 0;
}

void print_stack(stack* vm_stack) {
  for(int i = 0; i < vm_stack->head; i++) {
    printf("%ld\n", vm_stack->nodes[i].data);
  }
}

node_st stack_pop(stack* vm_stack, int count_inst) {
  if (empty(vm_stack)) {
    node_st elem;
    elem.data = 0;
    elem.type = OTHER;
    return elem;
  }
  return vm_stack->nodes[--(vm_stack->head)];
}

int stack_push(stack* vm_stack, node_st elem, int count_inst) {
  if (full(vm_stack)){
    printf("Cannot push, stack is full\n");
    return 1;
  }
  vm_stack->nodes[vm_stack->head] = elem;
  vm_stack->head++;
  return 0;
}

node_st peek(stack* vm_stack, int count_inst) {
  if (empty(vm_stack)) {
    node_st elem;
    elem.data = 0;
    elem.type = OTHER;
    return elem;
  }
  node_st elem = vm_stack->nodes[vm_stack->head-1];
  return elem;
}

node_hp* init_heap() {
  node_hp* heap = (node_hp*)malloc(HEAP_SIZE*sizeof(node_hp));
  return heap;
}

void DFS(node_hp* x){
  node_hp* t;
  long int y;
  int i = 0;
  if (!(x->is_marked)) {
    t = NULL;
    x->is_marked = 1;
    x->done = 0;
    while(1){
      i = x->done;
      if (i < 2) {
        y = x->fields[i];
        if (x->field_types[i] == POINTER && !(((node_hp*)y)->is_marked)) {
          x->fields[i] = (long int)t;
          t = x;
          x = (node_hp*)y;
          x->is_marked = 1;
          x->done = 0;
        }
        else x->done = i + 1;
      }
      else {
        y = (long int)x;
        x = t;
        if (x == NULL) return;
        i = x->done;
        t = (node_hp*)x->fields[i];
        x->fields[i] = y;
        x->done = i + 1;
      }
    }
  }
}

node_hp* mark_and_sweep(stack* vm_stack, node_hp* heap) {
  node_hp* free_list = NULL;
  for (int i = vm_stack->head - 1; i >= 0; i--)
    if (vm_stack->nodes[i].type == POINTER) {
      DFS((node_hp*)vm_stack->nodes[i].data);
    }
  for (int i = 0; i < HEAP_SIZE; i++) {
    if (heap[i].is_marked) {
      heap[i].is_marked = 0;
    }
    else {
      heap[i].fields[0] = (long int)free_list;
      free_list = &(heap[i]);
    }
  }
  return free_list;
}
