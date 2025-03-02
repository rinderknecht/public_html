
  public Stack<Item> rm_bot() { return tail.rm_bot_aux(head); }

  protected NStack<Item> rm_bot_aux(final Item item) {
    return rm_bot().push(item); }

  public NStack<Item> shuffle(final Stack<Item> stack) {
    return stack.shuffle(tail).push(head); }

  public NStack<Item> rep_fst() {
    return tail.rep_fst_aux(head).push(head); }

  protected NStack<Item> rep_fst_aux(final Item item) {
    return tail.rep_fst_aux(item).push(item); }

  public Stack<Item> merge(final Stack<Item> stack) {
    return stack.merge_aux(head,tail,this); }

  protected NStack<Item> merge_aux(final Item item,
                                   final Stack<Item> stack,
                                   final NStack<Item> orig) {
    return head.compareTo(item) <= 0 ?
           tail.merge(orig).push(head)
         : merge(stack).push(item);
  }

  public Int<Item> to_BST() { return tail.to_BST().add(head); }

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

