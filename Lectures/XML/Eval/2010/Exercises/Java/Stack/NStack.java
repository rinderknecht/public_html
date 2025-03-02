public class NStack<Item extends Comparable<? super Item>>
       extends Stack<Item> {
  protected final Item head;
  protected final Stack<Item> tail;

  public NStack(final Item item, final Stack<Item> stack) {
    head = item; tail = stack; }

  public Stack<Item> rev() { return rev_join(new EStack<Item>()); }

  public Stack<Item> rev_join(final Stack<Item> stack) {
    return tail.rev_join(stack.push(head)); }

  public NStack<Item> join(final Stack<Item> stack) {
    return tail.join(stack).push(head); }

  public Stack<Item> rm_bot() { return tail.rm_bot_aux(head); }

  protected NStack<Item> rm_bot_aux(final Item item) {
    return rm_bot().push(item); }

  public Stack<Item> rm_fst(final Item item) {
   return head.compareTo(item) == 0 ?
          tail
        : tail.rm_fst(item).push(head);
  }

  public Stack<Item> rm_lst(final Item item) {
    return head.compareTo(item) == 0 ?
           tail.rm_lst_aux(item,tail)
         : tail.rm_lst(item).push(head);
  }

  protected Stack<Item> rm_lst_aux(final Item item, 
                                   final Stack<Item> orig) {
    return head.compareTo(item) == 0 ?
           orig.rm_lst(item).push(head)
         : tail.rm_lst_aux(item,orig);
  }

  public NStack<Item> shuffle(final Stack<Item> stack) {
    return stack.shuffle(tail).push(head); }

  public NStack<Item> rep_fst() {
    return tail.rep_fst_aux(head).push(head); }

  protected NStack<Item> rep_fst_aux(final Item item) {
    return tail.rep_fst_aux(item).push(item); }

  public NStack<Item> isort() {
    return tail.isort().insert(head); }

  protected NStack<Item> insert(final Item item) {
    return head.compareTo(item) < 0 ?
           tail.insert(item).push(head)
         : push(item);
  }

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

  public void print() { System.out.print(head + " "); tail.print(); }
}
