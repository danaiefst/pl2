#include <stdlib.h>
#include <stdio.h>
#include <math.h>

#define SIZE_STACK 20000

typedef struct stack_t {
  int head;
  int data[SIZE_STACK];
} stack;

int full(stack* vm_stack){
  if (vm_stack->head == SIZE_STACK) {
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

int stack_pop(stack* vm_stack) {
  if (empty(vm_stack)) return 0;
  return vm_stack->data[--(vm_stack->head)];
}

int stack_push(stack* vm_stack, int elem) {
  if (full(vm_stack)){
    printf("Cannot push, stack is full\n");
    return 1;
  }
  vm_stack->data[vm_stack->head] = elem;
  vm_stack->head++;
  return 0;
}

int peek(stack* vm_stack) {
  if (empty(vm_stack)) return 0;
  return vm_stack->data[vm_stack->head-1];
}

void print_stack(stack* vm_stack) {
  for (int i = 0; i < vm_stack->head; i++) {
    printf("%c.", vm_stack->data[i]);
  }
  printf("\n");
}
