    Stack<Integer> s = (new EStack<Integer>()).push(5).push(2).push(7);
    s.print(); // 7 2 5
    s.rev().print(); // 5 2 7
    Stack<Integer> t = (new EStack<Integer>()).push(4).push(1);
    s.cat(t).print(); // 7 2 5 1 4
    s.cat(t).isort().print(); // 1 2 4 5 7

    s.isort().print(); // 1 3 3 5 7
    Stack<Integer> a = (new EStack<Integer>()).push(54).push(46).push(31).push(1);
    Stack<Integer> b = (new EStack<Integer>()).push(66).push(50).push(39);
    a.merge(b).print(); // 1 31 39 46 50 54 66
    a.to_BST().inorder(); // 1 31 46 54

    System.out.print(t.size() + "\n"); // 4 (#internal nodes in s)
    System.out.print((new Ext<Integer>()).size() + "\n"); // 0
    System.out.print(t.max() + "\n"); // 7
    t.to_stack().print(); // 0 1 5 7

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
