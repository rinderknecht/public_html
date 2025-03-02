///////////


// NStack.java
  public static <Item> Stack<Item> fromArray
    (final Item[] items, final int index, final Stack<Item> stack) {
    return index == 0 ? stack
           : fromArray(items,index-1,new NStack<Item>(items[index],stack));
  }

  public NStack(final Item[] items) throws EmptyArray {
    if (items.length == 0) throw new EmptyArray();
    head = items[0];
    tail = fromArray(items,items.length-1,new EStack<Item>());
  }
 
  public Item hd() { return head; }
  public Stack<Item> tl() { return tail; }

//SFunObj.java
    Stack<Integer> T = 
      new NStack<Integer>(3,
        new NStack<Integer>(4,
          new NStack<Integer>(5,
            new EStack<Integer>())));
    
     Stack<Integer> U = S.join(T);
     U.print();

     U.rev().print();

     Stack<Integer> V = U.rm_fst(4);
     V.print();

     try{
       Integer[] integers = {7,6,5,4,3,2,1};
       NStack<Integer> W = new NStack<Integer>(integers);
       W.print();
     }
     catch (EmptyArray exn) { 
       EStack<Integer> W = new EStack<Integer>();
       System.out.println("Exn: Empty array");
     }
