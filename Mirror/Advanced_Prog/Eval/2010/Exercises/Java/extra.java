// BST.java

  public BST<Item> flip();

// Ext.java

  public BST<Item> flip() { return this; }

// Int.java

  public BST<Item> flip() {
    return new Int<Item>(root,right.flip(),left.flip()); }

// BFunObj.java

    S.flip().inorder();
    System.out.println();
    System.out.print(S.height() + "\n");
