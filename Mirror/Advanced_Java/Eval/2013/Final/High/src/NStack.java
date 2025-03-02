public final class NStack<Key extends Comparable<Key>>
       extends Stack<Key> {

  private final Key head;
  private final Stack<Key> tail;

  protected NStack(final Key k, final Stack<Key> s) {
    head = k; tail = s; }

  public Stack<Key> merge(final Stack<Key> s) {
    return s.merge_aux(head,tail,this); }

  protected NStack<Key> merge_aux(final Key k,
                                  final Stack<Key> s,
                                  final NStack<Key> orig) {
    return head.compareTo(k) <= 0 ?
           tail.merge(orig).push(head)
         : merge(s).push(k);
  }

  public void print() { System.out.print(head + " "); tail.print(); }

}
