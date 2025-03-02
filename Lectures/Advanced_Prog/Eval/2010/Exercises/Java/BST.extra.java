  public             BST<Item> from_stack(final Stack<Item> s) {
                                          return s.to_BST(); }
  public    abstract boolean   mem(final Item i);

  public    abstract int size();
  public    abstract int height();
  public    abstract Item max() throws EmptyBST;
  protected abstract Item max_aux(final Item parent) throws EmptyBST;
  public    abstract Stack<Item> to_stack();
  protected abstract Stack<Item> to_stack_aux(final Stack<Item> stack);
