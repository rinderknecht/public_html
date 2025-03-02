  public    abstract BST<Item>    to_BST();
  public    abstract Stack<Item> rep_fst();
  protected abstract Stack<Item> rep_fst_aux(final Item item);
  public    abstract Stack<Item> merge(final Stack<Item> stack);
  protected abstract Stack<Item> merge_aux(final Item item,
                                           final Stack<Item> stack,
                                           final NStack<Item> orig);

