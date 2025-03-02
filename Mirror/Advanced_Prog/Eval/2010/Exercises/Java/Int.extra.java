  public boolean mem(final Item i) {
    int cmp = root.compareTo(i);
    if (cmp == 0) return true;
    if (cmp < 0) return left.mem(i);
    return right.mem(i);
  }
 
  public int size() { return 1 + left.size() + right.size(); }

  public int height () {
    final int lh = left.height();
    final int rh = right.height();
    return 1 + (lh > rh ? lh : rh);
  }

  public Item max() throws EmptyBST { return right.max_aux(root); }

  public Item max_aux(final Item parent) throws EmptyBST { return max(); }

  public Stack<Item> to_stack() {
    return to_stack_aux(new EStack<Item>()); }

  protected Stack<Item> to_stack_aux(final Stack<Item> stack) {
    return left.to_stack_aux(right.to_stack_aux(stack).push(root));
  }
