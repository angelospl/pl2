#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "time.h"
#define NEXT_INSTRUCTION goto *(void*)(label_tab[(int)(torus[pc.i][pc.j])])
#define N 25
#define M 80
#define STACKLENGTH 20
#define HEAPLENGTH 24

//-------------------USEFUL STRUCTS--------------------//
typedef struct node {
  signed long long int value;
  struct node* previous;
} node;

typedef struct cons_cell {
  signed long long int head;
  struct cons_cell* tail;
} cons_cell;

typedef struct heap_node {
  cons_cell value;
  struct heap_node* next;
} heap_node;

typedef struct prog {
  int i;
  int j;
} program_counter;

typedef enum direction{up,right,down,left} direction;

//------------------GLOBAL VARIABLES--------------------//
node* stack; //stack is a pointer to the head of the stack
unsigned long long int stack_elements;  //counter of stack elements
unsigned char torus[N][M]; //torus is a 2-dimensional 80x25 character array
program_counter pc; //program counter is 2 integers that show the pc's location at the torus
direction pc_movement;  //the direction pc is going
heap_node* heap; //heap is a pointer to a heap_node. We Cannot
            //have access to heap elements without having a pointer to them
unsigned long long int heap_elements; //counter of heap elements

//-----------------STACK IMPLEMENTATION----------------//
//checks if stack is empty
int isEmpty (){
  if (stack==NULL) return 1;
  else return 0;
}

//pops an element from the stack
signed long long int pop() {
  if (!isEmpty()) {
    signed long long int ret=stack->value;
    node* next_head= stack->previous;
    free (stack);
    stack=next_head;
    stack_elements--;
    return ret;
  }
  else {
    return 0;
  }
}

//pushes a signed long long int to the stack
void push (signed long long int x) {
  node* new_elem;
  if (stack_elements<pow(2,STACKLENGTH)) {
    new_elem=(node*) malloc(sizeof(node));
    new_elem->value=x;
    new_elem->previous=stack;
    stack=new_elem;
    stack_elements++;
  }
  else {
    printf("Push: Stack Overflow\n");
    exit(1);
  }
}

void empty_stack(){
  while(!isEmpty()) {
    pop();
  }
}


//------------------------HEAP FUNCTIONS--------------------//
void insert (signed long long int hd,cons_cell* tl){
  cons_cell* new_node;
  heap_node* new_heap_node;
  if (heap_elements<pow(2,HEAPLENGTH)){
    new_node=(cons_cell*)malloc(sizeof(cons_cell));
    new_node->head=hd;
    new_node->tail=tl;
    new_heap_node=(heap_node*)malloc(sizeof(heap_node));
    new_heap_node->value=*new_node;
    new_heap_node->next=heap;
    heap=new_heap_node;
    heap_elements++;
  }
  else {
    //garbage collection maybe
    ;
  }
}

void empty_heap () {
  heap_node* next;
  while (heap!=NULL) {
    next=heap->next;
    free(heap);
    heap=next;
    heap_elements--;
  }
}

//--------------------TORUS HANDLING FUNCTIONS-------------//
void read_torus(char const *arg) {
  FILE *fptr;
  int i,j;
  unsigned char character;
  fptr=fopen(arg,"r");
  if (fptr==NULL) {
    printf("Cannot open file\n");
    exit(1);
  }
  i=0;
  j=0;
  while (fscanf(fptr,"%c",&character)!=EOF) {
    if (j==M) {
      i++;
      j=0;
    }
    if (i==N) {
      printf("Very Large Program\n");
      exit(1);
    }
    if (character=='\n') {
      i++;
      j=0;
    }
    else {
        torus[i][j]=character;
        j++;
    }
  }
  //torus[i-1][j]='@';
}
void init_torus () {
  for (int i = 0; i < N; i++) {
    for (int j = 0; j < M; j++) {
      torus[i][j]=' ';
    }
  }
}
void print_torus () {
  for (int i = 0; i < N; i++) {
    if (i!=0) printf("\n");
    for (int j = 0; j < M; j++) {
      printf("%c",torus[i][j]);
    }
  }
  printf("\n");
}

//--------------------PROGRAM COUNTER MOVEMENT FUNCTIONS---------------//

void pc_left () {
  if (pc.j==0) pc.j=M-1;
  else pc.j--;
}

void pc_right () {
  if (pc.j==M-1) pc.j=0;
  else pc.j++;
}

void pc_up () {
  if (pc.i==0) pc.i=N-1;
  else pc.i--;
}

void pc_down (){
  if (pc.i==N-1) pc.i=0;
  else pc.i++;
}


void pc_move (){
  if (pc_movement==up) pc_up();
  else if (pc_movement==right) pc_right();
  else if (pc_movement==down) pc_down();
  else pc_left();
}

//----------------------THE BASIC FUNCTION OF THE VM------------------//
void run (){
  static void *label_tab[128];              //table with all the labels... for threaded code
  for (int i = 0; i< 128; i++) {
    if (i==32) label_tab[i]=&&space_label;            // space
    else if (i==33) label_tab[i]=&&not_label;         // !
    else if (i==34) label_tab[i]=&&stringmode_off_label;  // "
    else if (i==35) label_tab[i]=&&bridge_label;      // #
    else if (i==36) label_tab[i]=&&pop_label;         // $
    else if (i==37) label_tab[i]=&&mod_label;         // %
    else if (i==38) label_tab[i]=&&input_int_label;   // &
    else if (i==42) label_tab[i]=&&mul_label;         // *
    else if (i==43) label_tab[i]=&&plus_label;        // +
    else if (i==44) label_tab[i]=&&out_char_label;    // ,
    else if (i==45) label_tab[i]=&&minus_label;       // -
    else if (i==46) label_tab[i]=&&out_int_label;     // .
    else if (i==47) label_tab[i]=&&div_label;         // /
    else if (i==48) label_tab[i]=&&num0_label;        // 0-9
    else if (i==49) label_tab[i]=&&num1_label;
    else if (i==50) label_tab[i]=&&num2_label;
    else if (i==51) label_tab[i]=&&num3_label;
    else if (i==52) label_tab[i]=&&num4_label;
    else if (i==53) label_tab[i]=&&num5_label;
    else if (i==54) label_tab[i]=&&num6_label;
    else if (i==55) label_tab[i]=&&num7_label;
    else if (i==56) label_tab[i]=&&num8_label;
    else if (i==57) label_tab[i]=&&num9_label;
    else if (i==58) label_tab[i]=&&dup_label;         // :
    else if (i==60) label_tab[i]=&&left_label;        // <
    else if (i==62) label_tab[i]=&&right_label;       // >
    else if (i==63) label_tab[i]=&&rand_label;        // ?
    else if (i==64) label_tab[i]=&&end_label;         // @
    else if (i==92) label_tab[i]=&&swap_label;        /* \ */
    else if (i==94) label_tab[i]=&&up_label;          // ^
    else if (i==95) label_tab[i]=&&hor_if_label;      // _
    else if (i==96) label_tab[i]=&&greater_label;     // `
    else if (i==99) label_tab[i]=&&cons_label;        // c
    else if (i==103) label_tab[i]=&&get_label;        // g
    else if (i==104) label_tab[i]=&&head_label;       // h
    else if (i==112) label_tab[i]=&&put_label;        // p
    else if (i==116) label_tab[i]=&&tail_label;       // t
    else if (i==118) label_tab[i]=&&down_label;       // v
    else if (i==124) label_tab[i]=&&ver_if_label;     // |
    else if (i==126) label_tab[i]=&&input_char_label; // ~
    else label_tab[i]=&&def_label;
  }
  signed long long int val1,val2,x,y,value,cond;
  cons_cell* cell_ptr;
  heap_node* heap_ptr;
  int rnd,integ;
  unsigned char c;
  pc.i=0;
  pc.j=0;
  while (1) {
    unsigned opcode;
    opcode = torus[pc.i][pc.j];
    switch (opcode) {
      case '+':
      plus_label:
        pc_move();
        x=pop();
        y=pop();
        push(x+y);
        NEXT_INSTRUCTION;
      case '-':
      minus_label:
        pc_move();
        val2=pop();
        val1=pop();
        push(val1-val2);
        NEXT_INSTRUCTION;
      case '*':
      mul_label:
        pc_move();
        push(pop()*pop());
        NEXT_INSTRUCTION;
      case '/':
      div_label:
        val2=pop();
        val1=pop();
        pc_move();
        if (val2==0) {  //according to specifications the user chooses the result
          scanf("%lld\n",&val2 );
          push(val2);
        }
        else push(val1/val2);
        NEXT_INSTRUCTION;
      case '%':
      mod_label:
        val2=pop();
        val1=pop();
        pc_move();
        if (val2==0) {
          scanf("%lld\n",&val2 ); //according to specifications the user chooses the result
          push(val2);
        }
        else push(val1 % val2);
        NEXT_INSTRUCTION;
      case '!':
      not_label:
        pc_move();
        if (pop()==0) push(1);
        else push(0);
        NEXT_INSTRUCTION;
      case '`':
      greater_label:
        pc_move();
        val2=pop();
        val1=pop();
        if (val1 > val2) push(1);
        else push(0);
        NEXT_INSTRUCTION;
      case '>':
      right_label:
        pc_movement=right;
        pc_move();
        NEXT_INSTRUCTION;
      case '<':
      left_label:
        pc_movement=left;
        pc_move();
        NEXT_INSTRUCTION;
      case '^':
      up_label:
        pc_movement=up;
        pc_move();
        NEXT_INSTRUCTION;
      case 'v':
      down_label:
        pc_movement=down;
        pc_move();
        NEXT_INSTRUCTION;
      case '?':
      rand_label:
        rnd=rand()%4;
        if (rnd==0) pc_movement=up;
        else if (rnd==1) pc_movement=right;
        else if (rnd==2) pc_movement=down;
        else pc_movement=left;
        pc_move();
        NEXT_INSTRUCTION;
      case '_':
      hor_if_label:
        cond=pop();
        if (cond!=0) pc_movement=left;   //condition is true when is not zero
        else pc_movement=right;
        pc_move();
        NEXT_INSTRUCTION;
      case '|':
      ver_if_label:
        cond=pop();
        if (cond!=0) pc_movement=up;
        else pc_movement=down;
        pc_move();
        NEXT_INSTRUCTION;
      case '"':
      stringmode_off_label:
        goto stringmode_on_label;
      case ':':
      dup_label:
        pc_move();
        x=pop();
        push(x);
        push(x);
        NEXT_INSTRUCTION;
      case '\\':
      swap_label:
        pc_move();
        val2=pop();
        val1=pop();
        push(val2);
        push(val1);
        NEXT_INSTRUCTION;
      case '$':
      pop_label:
        pc_move();
        pop();
        NEXT_INSTRUCTION;
      case '.':
      out_int_label:
        printf("%lld ",pop());
        pc_move();
        NEXT_INSTRUCTION;
      case ',':
      out_char_label:
        printf("%c",(char)pop());
        pc_move();
        NEXT_INSTRUCTION;
      case '#':
      bridge_label:
        pc_move();
        pc_move();
        NEXT_INSTRUCTION;
      case ' ':
      space_label:
        pc_move();
        NEXT_INSTRUCTION;
      case 'g':
      get_label:
        x=pop();    //determines the row
        y=pop();    //determines the column
        if (x<N && y<M) {
          val1=torus[x][y];
          if ((val1>=32 && val1<39) || (val1>=42 && val1<59)
          || val1==60 || val1==62 || val1==63 || val1==64
          || val1==92 || val1==94 || val1==95 || val1==96 || val1==103
          || val1==112 || val1==118 || val1==124 || val1==126) {
            push((long int)val1);
          }
          else push(torus[x][y]);
        }
        else {      //if x y is out of bounds push(0) .. WIKIPEDIA EUXARISTO EXO OI MIZEROI APO DW
          push(0);
        }
        pc_move();
        NEXT_INSTRUCTION;
      case 'p':
      put_label:
        x=pop();
        y=pop();
        value=pop();
        if (x<N && y< M) torus[x][y]= (char)value;
        else {
          printf("x=%lld y=%lld Target out of bounds\n",x,y);
          exit(1);
        }
        pc_move();
        NEXT_INSTRUCTION;
      case 'c':               //pops y x creates cons cell (x,y) and pushes the address to the stack
      cons_label:
        cell_ptr=(cons_cell*)pop();
        x=pop();
        insert(x,cell_ptr);
        push((signed long long int) heap);
        pc_move();
        NEXT_INSTRUCTION;
      case 'h':
      head_label:
        heap_ptr=(heap_node*)pop();
        push(heap_ptr->value.head);
        pc_move();
        NEXT_INSTRUCTION;
      case 't':
      tail_label:
          heap_ptr=(heap_node*)pop();
          push((signed long long int)heap_ptr->value.tail);
          pc_move();
          NEXT_INSTRUCTION;
      case '&':
      input_int_label:
        scanf("%d", &integ);
        push((signed long long int) integ);
        pc_move();
        NEXT_INSTRUCTION;
      case '~':
      input_char_label:
        scanf("%c",&c );
        push((signed long long int) c);
        pc_move();
        NEXT_INSTRUCTION;
      case '0':
      num0_label:
        push(0);
        pc_move();
        NEXT_INSTRUCTION;
      case '1':
      num1_label:
        push(1);
        pc_move();
        NEXT_INSTRUCTION;
      case '2':
      num2_label:
        push(2);
        pc_move();
        NEXT_INSTRUCTION;
      case '3':
      num3_label:
        push(3);
        pc_move();
        NEXT_INSTRUCTION;
      case '4':
      num4_label:
        push(4);
        pc_move();
        NEXT_INSTRUCTION;
      case '5':
      num5_label:
        push(5);
        pc_move();
        NEXT_INSTRUCTION;
      case '6':
      num6_label:
        push(6);
        pc_move();
        NEXT_INSTRUCTION;
      case '7':
      num7_label:
        push(7);
        pc_move();
        NEXT_INSTRUCTION;
      case '8':
      num8_label:
        push(8);
        pc_move();
        NEXT_INSTRUCTION;
      case '9':
      num9_label:
        push(9);
        pc_move();
        NEXT_INSTRUCTION;
      case '@':
      end_label:
        //printf("\nTelos kalo ola kala\n");
        //print_torus();
        empty_stack();
        empty_heap();
        exit(0);
      case 'z':
      stringmode_on_label:
        pc_move();    //eixe meinei o pc sto stringmodeoff
        if (torus[pc.i][pc.j]!='"') {
          push((long int) torus[pc.i][pc.j]);
          goto stringmode_on_label;
        }
        else {
          pc_move();
          NEXT_INSTRUCTION;
        }

      // case 'w':
      // error_label:
      //   printf("Error at label tab\n");
      //   exit(1);
      default:      //unknown ASCII codes are ignored
      def_label:
        //push(torus[pc.i][pc.j]);
        printf("Error at label tab\n");       //currently if the pc encounters a non-command it terminates the program
        exit(1);
        pc_move();
        NEXT_INSTRUCTION;
    }
  }
}


//---------------------MAIN FUNCTION------------------//
int main(int argc, char const *argv[]) {
  if (argc!=2) {
    printf("Wrong arguments\n");
    exit(1);
  }
  stack_elements=0;
  heap_elements=0;
  init_torus ();
  read_torus(argv[1]);
  // print_torus();
  pc_movement=right;  //at start pc is going left to right
  srand(time(0));
  run();
  return 0;
}
