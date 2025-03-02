public class NStack<Item extends Comparable<? super Item>>
       extends Stack<Item> {
  protected final Item head;
  protected final Stack<Item> tail;

  public NStack(final Item item, final Stack<Item> stack) {
    head = item; tail = stack; }

  public NStack<Item> cat(final Stack<Item> stack) {
    return tail.cat(stack).push(head); }

  public void print() { System.out.print(head + " "); tail.print(); }

  public Stack<Item> rev_cat(final Stack<Item> stack) {
    return tail.rev_cat(stack.push(head)); }

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

  public NStack<Item> isort() { return tail.isort().insert(head); }

  protected NStack<Item> insert(final Item item) {
    return head.compareTo(item) < 0 ?
           tail.insert(item).push(head)
         : push(item);
  }

  public Pair<Stack<Item>,Stack<Item>> split (final int k) {
    if (k == 0)
       return new Pair<Stack<Item>,Stack<Item>>(new EStack<Item>(),this);
    Pair<Stack<Item>,Stack<Item>> p = tail.split(k-1);
    return new Pair<Stack<Item>,Stack<Item>>(p.fst().push(head),p.snd());
  }

  public Stack<Item> mrg(final Stack<Item> t) {
    return t.mrg0(head,tail); }

  public Stack<Item> mrg0(final Item x,final Stack<Item> s) {
      return x.compareTo(head) > 0 ?
             tail.mrg0(x,s).push(head)
           : s.mrg0(head,tail).push(x);
  }

  public Stack<Item> tms() { return tail.tms0(head); }

  protected Stack<Item> tms0(final Item x) {
    return tail.cutr(new EStack<Item>().push(x),this); }

  public Stack<Item> cutr(final Stack<Item> s,
                          final Stack<Item> t) {
    return tail.cutr0(s,t); }

  protected Stack<Item> cutr0(final Stack<Item> s,
                              final Stack<Item> t) {
    return t.cutr1(s,tail); }

  protected Stack<Item> cutr1(final Stack<Item> s,
                              final Stack<Item> u) {
    return u.cutr(s.push(head),tail); }
}
