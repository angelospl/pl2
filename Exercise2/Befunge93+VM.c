#include <stdio.h>
#include <stdlib.h>
#include<stdint.h>
#include <math.h>
#include "time.h"
#define NEXT_INSTRUCTION goto *(void*)(label_tab[(int)(torus[pc.i][pc.j])])
#define N 25
#define M 80
#define STACKLENGTH 20
#define HEAPLENGTH 24
#define OLDHEAPLENGTH 23

//-------------------USEFUL STRUCTS--------------------//
typedef enum bef_type {Pointer,Integer} bef_type;

typedef enum {old,new} generation;

typedef struct node {
  signed long long int value;
  bef_type type;
  struct node* previous;
} node;

typedef struct heap_node {    //heap_node is a double linked list,
  signed long long int head;  //the head of the cons cell
  struct heap_node* tail;     //the tail of the cons cell
  uint8_t marked;             //marked by garbage collection
  generation gen;         //will be used for generational algorithm
  struct heap_node* next;     //the pointers the double linking
  struct heap_node* previous;
} heap_node;

typedef struct list_node {
  heap_node* pointer;
  struct list_node* next;
} list_node;

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
heap_node* heap,*freelist,*old_heap;  //heap is a pointer to a heap_node. We Cannot
                            //have access to heap elements without having a pointer to them
                            //freelist is the list of the nodes to be garbage collected
unsigned long long int heap_elements,old_heap_elements; //counter of heap elements
bef_type pop_ret;           //stores the type of elements popped from the stack. helps garbage collector
                            //find root nodes
list_node* entry_table;

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
    pop_ret=stack->type;
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
void push (signed long long int x,bef_type typ) {
  node* new_elem;
  if (stack_elements<(1<<STACKLENGTH)) {
    new_elem=(node*) malloc(sizeof(node));
    new_elem->value=x;
    new_elem->previous=stack;
    new_elem->type=typ;
    stack=new_elem;
    stack_elements++;
  }
  else {
    printf("Push: Stack Overflow\n");
    exit(1);
  }
}

//pops all elements from the stack
void empty_stack(){
  while(!isEmpty()) {
    pop();
  }
}

//-----------------------ENTRY TABLE-------------------------//
void add_new_entry(heap_node* ptr){
  list_node* new_node;
  new_node=(list_node*)malloc(sizeof(list_node));
  new_node->pointer=ptr;
  new_node->next=entry_table;
  entry_table=new_node;
}

void empty_entry_table () {
  list_node* node;
  while (entry_table!=NULL) {
    node=entry_table;
    entry_table=entry_table->next;
    free(node);
  }
}

//-----------------------GARBAGE COLLECTION------------------//

//takes a new heap elem and takes it to the old heap
void grow_old (heap_node* x) {
  x->gen=old;
  if (x->next!=NULL) {              //pop from the heap
    x->next->previous=x->previous;
  }
  if (x->previous!=NULL) {
    x->previous->next=x->next;
  }
  heap_elements--;
  x->next=old_heap;               //insert in the old heap
  x->previous=NULL;
  if (x->next!=NULL) {
    x->next->previous=x;
  }
  old_heap=x;
  old_heap_elements++;
}


//dfs traversal to the nodes of a tree starting from the root nodes
void DFS (heap_node* x,generation gen){
  if (x==NULL) {
    return ;
  }
  if (gen==x->gen) {
    if (x->marked==0) {                       //0 means not marked
      x->marked=1;
      DFS(x->tail,gen);
    }
  }
  // else if (gen==old) {       //if we do gc to the old heap. and we find an element of the new heap
  //                           //we keep that pointer because it points from older to newer generation
  //   add_new_entry(x);       //add to the entry list      //PROBABLY SHOULDNT BE DONE HERE
  // }
}

//iterates through the stack finds pointers and calls dfs on them
void mark (generation gen){
  heap_node* heap_elem;
  list_node* list_iter;
  node* iter=stack;
  while (iter!=NULL) {
    if (iter->type==Pointer) {
      heap_elem=(heap_node*)iter->value;
      DFS(heap_elem,gen);
    }
    iter=iter->previous;
  }
  if (gen==new) {             //for the new generation we have to look also at the entry table
    list_iter=entry_table;    //to see the pointers from old to new generation
    while (list_iter!=NULL) {
      DFS(list_iter->pointer,gen);
      list_iter=list_iter->next;
    }
  }
}

//sweeps all garbage marked by the mark function
void sweep (generation gen) {
  heap_node* heap_iter,*next;
  freelist=NULL;
  if (gen==new) heap_iter=heap;   //checks for generation and iterates the right heap
  else heap_iter=old_heap;
  while (heap_iter!=NULL) {     //iterate through the heap
    next=heap_iter->next;
    if (heap_iter->marked==1) {
      heap_iter->marked=0;      //unmark
      if (heap_iter->gen==new) {  //whichever object survives gc becomes old
        grow_old(heap_iter);
      }
    }
    else {
      heap_iter->tail=freelist;
      freelist=heap_iter;
    }
    heap_iter=next;
  }
  heap_iter=NULL;
  while (freelist!=NULL) {
    heap_iter=freelist;
    freelist=freelist->tail;
    if (heap_iter->next!=NULL) {                      //pointers have to point to the correct elements after
      heap_iter->next->previous=heap_iter->previous;  //the collection of a garbage from the heap
    }
    if (heap_iter->previous!=NULL) {
      heap_iter->previous->next=heap_iter->next;
    }
    else heap=heap_iter->next;
    free(heap_iter);
    heap_elements--;
    //printf("mpika\n");
  }
  heap_iter=NULL;
}

//------------------------HEAP FUNCTIONS--------------------//

//inserts to the top of the heap a cons cell (hd,tl). it is unmarked and generation is 0
void insert (signed long long int hd,heap_node* tl){
  heap_node* new_heap_node;
  new_heap_node=(heap_node*)malloc(sizeof(heap_node));
  new_heap_node->head=hd;
  new_heap_node->tail=tl;
  new_heap_node->previous=NULL;
  new_heap_node->marked=0;
  new_heap_node->gen=new;
  new_heap_node->next=heap;
  if (new_heap_node->next!=NULL) {
    new_heap_node->next->previous=new_heap_node;
  }
  heap=new_heap_node;
  heap_elements++;
}

//not used garbage collector is called at the end of the program
void empty_heap (generation gen) {
  heap_node* next,*node;
  if (gen==old) node=old_heap;
  else node=heap;
  while (node!=NULL) {
    next=node->next;
    free(node);
    node=next;
    if (gen==old) old_heap_elements--;
    else heap_elements--;
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
  signed long long int val1,val2,x,y,value,cond,integ;
  heap_node* heap_ptr,*cell_ptr;
  int rnd;
  unsigned char c;
  bef_type typ1,typ2;
  pc.i=0;
  pc.j=0;
  entry_table=NULL;
  while (1) {
    unsigned opcode;
    opcode = torus[pc.i][pc.j];
    switch (opcode) {
      case '+':
      plus_label:
        pc_move();
        x=pop();
        y=pop();
        push(x+y,Integer);
        NEXT_INSTRUCTION;
      case '-':
      minus_label:
        pc_move();
        val2=pop();
        val1=pop();
        push(val1-val2,Integer);
        NEXT_INSTRUCTION;
      case '*':
      mul_label:
        pc_move();
        push(pop()*pop(),Integer);
        NEXT_INSTRUCTION;
      case '/':
      div_label:
        val2=pop();
        val1=pop();
        pc_move();
        if (val2==0) {  //according to specifications the user chooses the result
          scanf("%lld\n",&val2 );
          push(val2,Integer);
        }
        else push(val1/val2,Integer);
        NEXT_INSTRUCTION;
      case '%':
      mod_label:
        val2=pop();
        val1=pop();
        pc_move();
        if (val2==0) {
          scanf("%lld\n",&val2 ); //according to specifications the user chooses the result
          push(val2,Integer);
        }
        else push(val1 % val2,Integer);
        NEXT_INSTRUCTION;
      case '!':
      not_label:
        pc_move();
        if (pop()==0) push(1,Integer);
        else push(0,Integer);
        NEXT_INSTRUCTION;
      case '`':
      greater_label:
        pc_move();
        val2=pop();
        val1=pop();
        if (val1 > val2) push(1,Integer);
        else push(0,Integer);
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
        push(x,pop_ret);
        push(x,pop_ret);
        NEXT_INSTRUCTION;
      case '\\':
      swap_label:
        pc_move();
        val2=pop();
        typ2=pop_ret;
        val1=pop();
        typ1=pop_ret;
        push(val2,typ2);
        push(val1,typ1);
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
            push((long long int)val1,Integer);
          }
          else push(torus[x][y],Integer);
        }
        else {      //if x y is out of bounds push(0) .. WIKIPEDIA EUXARISTO EXO OI MIZEROI APO DW
          push(0,Integer);
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
        cell_ptr=(heap_node*)pop();
        x=pop();
        insert(x,cell_ptr);
        push((signed long long int) heap,Pointer);
        if (heap_elements+(1<<(OLDHEAPLENGTH))>=1<<(HEAPLENGTH)){
          mark(new);
          sweep(new);
          if (old_heap_elements>=(1<<OLDHEAPLENGTH)) {
            mark(old);
            sweep(old);
          }
        }
        pc_move();
        NEXT_INSTRUCTION;
      case 'h':
      head_label:
        heap_ptr=(heap_node*)pop();
        push(heap_ptr->head,Integer);
        pc_move();
        NEXT_INSTRUCTION;
      case 't':
      tail_label:
          heap_ptr=(heap_node*)pop();
          push((signed long long int)heap_ptr->tail,Pointer);
          pc_move();
          NEXT_INSTRUCTION;
      case '&':
      input_int_label:
        scanf("%lld", &integ);
        push((signed long long int) integ,Integer);
        pc_move();
        NEXT_INSTRUCTION;
      case '~':
      input_char_label:
        scanf("%c",&c );
        push((signed long long int) c,Integer);
        pc_move();
        NEXT_INSTRUCTION;
      case '0':
      num0_label:
        push(0,Integer);
        pc_move();
        NEXT_INSTRUCTION;
      case '1':
      num1_label:
        push(1,Integer);
        pc_move();
        NEXT_INSTRUCTION;
      case '2':
      num2_label:
        push(2,Integer);
        pc_move();
        NEXT_INSTRUCTION;
      case '3':
      num3_label:
        push(3,Integer);
        pc_move();
        NEXT_INSTRUCTION;
      case '4':
      num4_label:
        push(4,Integer);
        pc_move();
        NEXT_INSTRUCTION;
      case '5':
      num5_label:
        push(5,Integer);
        pc_move();
        NEXT_INSTRUCTION;
      case '6':
      num6_label:
        push(6,Integer);
        pc_move();
        NEXT_INSTRUCTION;
      case '7':
      num7_label:
        push(7,Integer);
        pc_move();
        NEXT_INSTRUCTION;
      case '8':
      num8_label:
        push(8,Integer);
        pc_move();
        NEXT_INSTRUCTION;
      case '9':
      num9_label:
        push(9,Integer);
        pc_move();
        NEXT_INSTRUCTION;
      case '@':
      end_label:
        //printf("\nTelos kalo ola kala\n");
        //print_torus();
        empty_stack();      //at the end we empty the stack and then we call the garbage collector
        empty_entry_table();
        mark(new);
        sweep(new);
        mark(old);
        sweep(old);
        //empty_heap();
        exit(0);
      case 'z':
      stringmode_on_label:
        pc_move();    //eixe meinei o pc sto stringmodeoff
        if (torus[pc.i][pc.j]!='"') {
          push((long int) torus[pc.i][pc.j],Integer);
          goto stringmode_on_label;
        }
        else {
          pc_move();
          NEXT_INSTRUCTION;
        }
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
