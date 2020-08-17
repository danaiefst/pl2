#include <stdio.h>
#include <stdlib.h>
#include "stack.c"
#include <time.h>
#define RIGHT 0
#define LEFT 1
#define UP 2
#define DOWN 3
#define RANDOM 4
#define NORM_MODE 0
#define STR_MODE 1
#define NEXT_INSTRUCTION move(); /*print_stack(vm_stack); printf("input_mode = %d, program[x][y] = %c, coords %d, %d\n", input_mode, program[x][y], x, y);*/ if(input_mode){ if (program[x][y] == '"') goto str_mode_label; else goto push_label;} else {if (program_labels[x][y] == 28) {printf("%c is not recognized as a Befunge-93 command\n", program[x][y]); return 1;} else goto *(void *)(label_tab[program_labels[x][y]]);}

stack* vm_stack;
int direction = RIGHT;
int x = 0;
int y = 0;
int push_type;
int input_mode = NORM_MODE;
int n, m = 0;

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
  int a = stack_pop(vm_stack);
  int b = stack_pop(vm_stack);
  return stack_push(vm_stack, a+b);
}

int subtract(){
  int a = stack_pop(vm_stack);
  int b = stack_pop(vm_stack);
  return stack_push(vm_stack, b-a);
}

int multiply(){
  int a = stack_pop(vm_stack);
  int b = stack_pop(vm_stack);
  return stack_push(vm_stack, a*b);
}

int divide(){
  int a = stack_pop(vm_stack);
  int b = stack_pop(vm_stack);
  return stack_push(vm_stack, b/a);
}

int modulo(){
  int a = stack_pop(vm_stack);
  int b = stack_pop(vm_stack);
  return stack_push(vm_stack, b%a);
}

int not(){
  int a = stack_pop(vm_stack);
  return stack_push(vm_stack, !a);
}

int greater(){
  int a = stack_pop(vm_stack);
  int b = stack_pop(vm_stack);
  return stack_push(vm_stack, b>a);
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
  //printf("changed dir, new dir %d\n", direction);
  return 0;
}

int rand_dir(){
  direction = RANDOM;
  return 0;
}

int horizontal_if(){
  int a = stack_pop(vm_stack);
  direction = a ? LEFT : RIGHT;
  return 0;
}

int vertical_if(){
  int a = stack_pop(vm_stack);
  direction = a ? UP : DOWN;
  return 0;
}

int stringmode(){
  input_mode = !input_mode;
  //printf("changed mode, new mode %d\n", input_mode);
  return 0;
}

int dup(){
  int a = peek(vm_stack);
  return stack_push(vm_stack, a);
}

int swap(){
  int a = stack_pop(vm_stack);
  int b = stack_pop(vm_stack);
  int ret_1 = stack_push(vm_stack, a);
  int ret_2 = stack_push(vm_stack, b);
  return min(ret_1, ret_2);
}

int pop(){
  stack_pop(vm_stack);
  return 0;
}

int push(int val){
  return stack_push(vm_stack, val);
}

int pop_int(){
  printf("%d ", stack_pop(vm_stack));
  return 0;
}

int pop_ascii(){
  printf("%c", stack_pop(vm_stack));
  return 0;
}

int bridge(){
  move();
  return 0;
}

int g(int n, int m, char program[n][m]) {
  int b = stack_pop(vm_stack);
  int a = stack_pop(vm_stack);
  return stack_push(vm_stack, program[b][a]);
}

int p(int n, int m, char program[n][m]) {
  int b = stack_pop(vm_stack);
  int a = stack_pop(vm_stack);
  int val = stack_pop(vm_stack);
  program[b][a] = val;
  return 0;
}

int input_int(){
  int a;
  scanf("%d", &a);
  return stack_push(vm_stack, a);
}

int input_ascii(){
  char a;
  scanf("%c", &a);
  return stack_push(vm_stack, a);
}

int number(int val){
  return stack_push(vm_stack, val - 48);
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
  };
  vm_stack = (stack*)malloc(sizeof(stack));
  vm_stack->head = 0;
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
        default:
          program_labels[i][j] = 28;
          break;
      }
    }
    if(in != '\n') fread(&in, 1, 1, fd);
  }

  //printf("input_mode = %d, program[x][y] = %c\n", input_mode, program[x][y]);
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
  //int flag = 0;
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
  //flag++;
  if(horizontal_if()) return 1;
  //if (flag == 10) return 0;
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
  //printf("in\n");
  if(input_int()) return 1;
  NEXT_INSTRUCTION;

  in_ascii_label:
  if(input_ascii()) return 1;
  NEXT_INSTRUCTION;

  space_label:
  NEXT_INSTRUCTION;

  num_label:
  //printf("ok1\n");
  if(number(program[x][y])) return 1;
  //printf("ok2\n");
  NEXT_INSTRUCTION;

  end_label:
  return 0;

  push_label:
  if(push(program[x][y])) return 1;
  NEXT_INSTRUCTION;

  return 0;
}
