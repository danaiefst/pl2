#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "heap.c"

#define RIGHT 0
#define LEFT 1
#define UP 2
#define DOWN 3
#define RANDOM 4
#define NORM_MODE 0
#define STR_MODE 1
#define NEXT_INSTRUCTION move(); if(input_mode){ if (program[x][y] == '"') goto str_mode_label; else goto push_label;} else {if (program_labels[x][y] == 28) {printf("%c is not recognized as a Befunge-93 command\n", program[x][y]); return 1;} else goto *(void *)(label_tab[program_labels[x][y]]);}

stack* vm_stack;
node_hp* heap;
node_hp* free_list = NULL;
int direction = RIGHT;
int x = 0;
int y = 0;
int push_type;
int input_mode = NORM_MODE;
int n, m = 0;
int heap_iter = 0;
int count_inst = 0;
int flag = 0;
FILE *fd1;

int min(int a, int b){
  if (a<b) return a;
  return b;
}

void move(){
  if (direction == RIGHT) {
    y = (y + 1) % m;
  }
  else if (direction == LEFT) {
    y = (m + y - 1) % m;
  }
  else if (direction == UP) {
    x = (n + x - 1) % n;
  }
  else if (direction == DOWN) {
    x = (x + 1) % n;
  }
  else {
    direction = rand() % 4;
    move();
  }
}

int add(){
  node_st a = stack_pop(vm_stack, count_inst);
  node_st b = stack_pop(vm_stack, count_inst);
  a.data += b.data;
  a.type = OTHER;
  if (a.type == POINTER || b.type == POINTER) {
    a.type = POINTER;
    return stack_push(vm_stack, a, count_inst);
  }
  return stack_push(vm_stack, a, count_inst);
}

int subtract(){
  node_st b = stack_pop(vm_stack, count_inst);
  node_st a = stack_pop(vm_stack, count_inst);
  a.data -= b.data;
  a.type = OTHER;
  if (a.type == POINTER || b.type == POINTER) {
    a.type = POINTER;
    return stack_push(vm_stack, a, count_inst);
  }
  return stack_push(vm_stack, a, count_inst);
}

int multiply(){
  node_st a = stack_pop(vm_stack, count_inst);
  node_st b = stack_pop(vm_stack, count_inst);
  a.data *= b.data;
  a.type = OTHER;
  if (a.type == POINTER || b.type == POINTER) {
    a.type = POINTER;
    return stack_push(vm_stack, a, count_inst);
  }
  return stack_push(vm_stack, a, count_inst);
}

int divide(){
  node_st b = stack_pop(vm_stack, count_inst);
  node_st a = stack_pop(vm_stack, count_inst);
  a.data /= b.data;
  a.type = OTHER;
  if (a.type == POINTER || b.type == POINTER) {
    a.type = POINTER;
    return stack_push(vm_stack, a, count_inst);
  }
  return stack_push(vm_stack, a, count_inst);
}

int modulo(){
  node_st b = stack_pop(vm_stack, count_inst);
  node_st a = stack_pop(vm_stack, count_inst);
  a.data %= b.data;
  a.type = OTHER;
  if (a.type == POINTER || b.type == POINTER) {
    a.type = POINTER;
    return stack_push(vm_stack, a, count_inst);
  }
  return stack_push(vm_stack, a, count_inst);
}

int not(){
  node_st a = stack_pop(vm_stack, count_inst);
  a.data = !a.data;
  a.type = OTHER;
  return stack_push(vm_stack, a, count_inst);
}

int greater(){
  node_st a = stack_pop(vm_stack, count_inst);
  node_st b = stack_pop(vm_stack, count_inst);
  a.data = b.data > a.data;
  a.type = OTHER;
  return stack_push(vm_stack, a, count_inst);
}

int right(){
  direction = RIGHT;
  return 0;
}

int left(){
  direction = LEFT;
  return 0;
}

int up(){
  direction = UP;
  return 0;
}

int down(){
  direction = DOWN;
  return 0;
}

int rand_dir(){
  direction = RANDOM;
  return 0;
}

int horizontal_if(){
  node_st a = stack_pop(vm_stack, count_inst);
  direction = a.data ? LEFT : RIGHT;
  return 0;
}

int vertical_if(){
  node_st a = stack_pop(vm_stack, count_inst);
  direction = a.data ? UP : DOWN;
  return 0;
}

int stringmode(){
  input_mode = !input_mode;
  return 0;
}

int dup(){
  node_st a = peek(vm_stack, count_inst);
  return stack_push(vm_stack, a, count_inst);
}

int swap(){
  node_st a = stack_pop(vm_stack, count_inst);
  node_st b = stack_pop(vm_stack, count_inst);
  int ret_1 = stack_push(vm_stack, a, count_inst);
  int ret_2 = stack_push(vm_stack, b, count_inst);
  return min(ret_1, ret_2);
}

int pop(){
  stack_pop(vm_stack, count_inst);
  return 0;
}

int push(int val){
  node_st elem;
  elem.data = val;
  elem.type = OTHER;
  return stack_push(vm_stack, elem, count_inst);
}

int pop_int(){
  printf("%ld ", stack_pop(vm_stack, count_inst).data);
  return 0;
}

int pop_ascii(){
  printf("%c", (char)stack_pop(vm_stack, count_inst).data);
  return 0;
}

int bridge(){
  move();
  return 0;
}

int g(int n, int m, char program[n][m]) {
  node_st b = stack_pop(vm_stack, count_inst);
  node_st a = stack_pop(vm_stack, count_inst);
  node_st elem;
  elem.data = program[(int)b.data][(int)a.data];
  elem.type = OTHER;
  return stack_push(vm_stack, elem, count_inst);
}

int p(int n, int m, char program[n][m]) {
  node_st b = stack_pop(vm_stack, count_inst);
  node_st a = stack_pop(vm_stack, count_inst);
  node_st val = stack_pop(vm_stack, count_inst);
  program[b.data][a.data] = val.data;
  return 0;
}

int input_int(){
  node_st elem;
  scanf("%ld", &elem.data);
  elem.type = OTHER;
  return stack_push(vm_stack, elem, count_inst);
}

int input_ascii(){
  node_st elem;
  scanf("%ld", &elem.data);
  elem.type = OTHER;
  return stack_push(vm_stack, elem, count_inst);
}

int number(int val){
  node_st elem;
  elem.data = val - 48;
  elem.type = OTHER;
  return stack_push(vm_stack, elem, count_inst);
}

int cons() {
  node_st a, b;
  node_st elem;
  if(heap_iter >= HEAP_SIZE) {
    if(free_list == NULL) {
      free_list = mark_and_sweep(vm_stack, heap);
    }
    b = stack_pop(vm_stack, count_inst);
    a = stack_pop(vm_stack, count_inst);
    node_hp* temp = free_list;
    free_list = (node_hp*)free_list->fields[0];
    temp->fields[0] = a.data;
    temp->fields[1] = b.data;
    temp->field_types[0] = a.type;
    temp->field_types[1] = b.type;
    temp->is_marked = 0;
    elem.data = (long int)temp;
  }
  else {
    b = stack_pop(vm_stack, count_inst);
    a = stack_pop(vm_stack, count_inst);
    heap[heap_iter].fields[0] = a.data;
    heap[heap_iter].fields[1] = b.data;
    heap[heap_iter].field_types[0] = a.type;
    heap[heap_iter].field_types[1] = b.type;
    heap[heap_iter].is_marked = 0;
    elem.data = (long int)&(heap[heap_iter]);
    heap_iter++;
  }
  elem.type = POINTER;
  return stack_push(vm_stack, elem, count_inst);
}

int head() {
  if(peek(vm_stack, count_inst).type != POINTER) {
    printf("head operation expects pointer on top of stack\n");
    return 1;
  }
  node_st elem;
  node_st temp = stack_pop(vm_stack, count_inst);
  elem.data = (((node_hp*)temp.data)->fields[0]);
  elem.type = (((node_hp*)temp.data)->field_types[0]);
  return stack_push(vm_stack, elem, count_inst);
}

int tail() {
  if(peek(vm_stack, count_inst).type != POINTER) {
    printf("tail operation expects pointer on top of stack\n");
    return 1;
  }
  node_st elem;
  node_hp* p = (node_hp*)stack_pop(vm_stack, count_inst).data;
  elem.data = p->fields[1];
  elem.type = p->field_types[1];
  return stack_push(vm_stack, elem, count_inst);
}

void initialize(int n, int m, char program[n][m]) {
  for(int i = 0; i < n; i++) {
    for(int j = 0; j < m; j++) {
      program[i][j] = ' ';
    }
  }
}

int main(int argc, char *argv[]) {
  static void *label_tab[] = {
    &&num_label,
    &&add_label,
    &&sub_label,
    &&mult_label,
    &&div_label,
    &&mod_label,
    &&not_label,
    &&greater_label,
    &&right_label,
    &&left_label,
    &&up_label,
    &&down_label,
    &&rand_dir_label,
    &&horiz_if_label,
    &&vert_if_label,
    &&str_mode_label,
    &&dup_label,
    &&swap_label,
    &&pop_label,
    &&pop_int_label,
    &&pop_ascii_label,
    &&bridge_label,
    &&p_label,
    &&g_label,
    &&in_int_label,
    &&in_ascii_label,
    &&end_label,
    &&space_label,
    &&push_label,
    &&cons_label,
    &&head_label,
    &&tail_label
  };
  vm_stack = init_stack(pow(2,20));
  heap = init_heap();
  srand(time(0));
  FILE *fd = fopen(argv[1], "rb");

  if (!fd) {
    printf("Error opening file\n");
    return 1;
  }

  char val;
  int temp_m = 0;
  while(fread(&val, 1, 1, fd) > 0){
    if (val == '\n'){
      n++;
      m = temp_m > m ? temp_m : m;
      temp_m = 0;
    }
    else temp_m++;
  }

  char program[n][m];
  int program_labels[n][m];
  rewind(fd);
  initialize(n, m, program);
  char in;

  for(int i = 0; i < n; i++) {
    for(int j = 0; j < m; j++) {
      fread(&in, 1, 1, fd);
      if(in == '\n'){
        break;
      }
      program[i][j] = in;
      switch (program[i][j]) {
        case '+':
          program_labels[i][j] = 1;
          break;
        case '-':
          program_labels[i][j] = 2;
          break;
        case '*':
          program_labels[i][j] = 3;
          break;
        case '/':
          program_labels[i][j] = 4;
          break;
        case '%':
          program_labels[i][j] = 5;
          break;
        case '!':
          program_labels[i][j] = 6;
          break;
        case '`':
          program_labels[i][j] = 7;
          break;
        case '>':
          program_labels[i][j] = 8;
          break;
        case '<':
          program_labels[i][j] = 9;
          break;
        case '^':
          program_labels[i][j] = 10;
          break;
        case 'v':
          program_labels[i][j] = 11;
          break;
        case '?':
          program_labels[i][j] = 12;
          break;
        case '_':
          program_labels[i][j] = 13;
          break;
        case '|':
          program_labels[i][j] = 14;
          break;
        case '"':
          program_labels[i][j] = 15;
          break;
        case ':':
          program_labels[i][j] = 16;
          break;
        case '\\':
          program_labels[i][j] = 17;
          break;
        case '$':
          program_labels[i][j] = 18;
          break;
        case '.':
          program_labels[i][j] = 19;
          break;
        case ',':
          program_labels[i][j] = 20;
          break;
        case '#':
          program_labels[i][j] = 21;
          break;
        case 'p':
          program_labels[i][j] = 22;
          break;
        case 'g':
          program_labels[i][j] = 23;
          break;
        case '&':
          program_labels[i][j] = 24;
          break;
        case '~':
          program_labels[i][j] = 25;
          break;
        case ' ':
          program_labels[i][j] = 27;
          break;
        case '0' ... '9':
          program_labels[i][j] = 0;
          break;
        case '@':
          program_labels[i][j] = 26;
          break;
        case 'c':
          program_labels[i][j] = 29;
          break;
        case 'h':
          program_labels[i][j] = 30;
          break;
        case 't':
          program_labels[i][j] = 31;
          break;
        default:
          program_labels[i][j] = 28;
          break;
      }
    }
    if(in != '\n') fread(&in, 1, 1, fd);
  }

  if(input_mode){
    if (program[x][y] == '"') goto str_mode_label;
    else goto push_label;
  }
  else {
    if (program_labels[x][y] == 28) {
      printf("%c is not recognized as a Befunge-93 command\n", program[x][y]);
      return 1;
    }
    else goto *(void *)(label_tab[program_labels[x][y]]);
  }

  add_label:
  if(add()) return 1;
  NEXT_INSTRUCTION;

  sub_label:
  if(subtract()) return 1;
  NEXT_INSTRUCTION;

  mult_label:
  if(multiply()) return 1;
  NEXT_INSTRUCTION;

  div_label:
  if(divide()) return 1;
  NEXT_INSTRUCTION;

  mod_label:
  if(modulo()) return 1;
  NEXT_INSTRUCTION;

  not_label:
  if(not()) return 1;
  NEXT_INSTRUCTION;

  greater_label:
  if(greater()) return 1;
  NEXT_INSTRUCTION;

  right_label:
  if(right()) return 1;
  NEXT_INSTRUCTION;

  left_label:
  if(left()) return 1;
  NEXT_INSTRUCTION;

  up_label:
  if(up()) return 1;
  NEXT_INSTRUCTION;

  down_label:
  if(down()) return 1;
  NEXT_INSTRUCTION;

  rand_dir_label:
  if(rand_dir()) return 1;
  NEXT_INSTRUCTION;

  horiz_if_label:
  if(horizontal_if()) return 1;
  NEXT_INSTRUCTION;

  vert_if_label:
  if(vertical_if()) return 1;
  NEXT_INSTRUCTION;

  str_mode_label:
  if(stringmode()) return 1;
  NEXT_INSTRUCTION;

  dup_label:
  if(dup()) return 1;
  NEXT_INSTRUCTION;

  swap_label:
  if(swap()) return 1;
  NEXT_INSTRUCTION;

  pop_label:
  if(pop()) return 1;
  NEXT_INSTRUCTION;

  pop_int_label:
  if(pop_int()) return 1;
  NEXT_INSTRUCTION;

  pop_ascii_label:
  if(pop_ascii()) return 1;
  NEXT_INSTRUCTION;

  bridge_label:
  if(bridge()) return 1;
  NEXT_INSTRUCTION;

  p_label:
  if(p(n, m, program)) return 1;
  NEXT_INSTRUCTION;

  g_label:
  if(g(n, m, program)) return 1;
  NEXT_INSTRUCTION;

  in_int_label:
  if(input_int()) return 1;
  NEXT_INSTRUCTION;

  in_ascii_label:
  if(input_ascii()) return 1;
  NEXT_INSTRUCTION;

  space_label:
  NEXT_INSTRUCTION;

  num_label:
  if(number(program[x][y])) return 1;
  NEXT_INSTRUCTION;

  end_label:
  return 0;

  push_label:
  if(push(program[x][y])) return 1;
  NEXT_INSTRUCTION;

  cons_label:
  if(cons()) return 1;
  NEXT_INSTRUCTION;

  head_label:
  if(head()) return 1;
  NEXT_INSTRUCTION;

  tail_label:
  if(tail()) return 1;
  NEXT_INSTRUCTION;

  return 0;
}
