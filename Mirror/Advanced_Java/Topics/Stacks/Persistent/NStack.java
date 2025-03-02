public final class NStack<Key extends Comparable<Key>>
       extends Stack<Key> {

  private final Key head;
  private final Stack<Key> tail;

  protected NStack(final Key k, final Stack<Key> s) {
    head = k; tail = s; }

  public final boolean isEmpty () { return false; }

  public NStack<Key> cat(final Stack<Key> s) {
    return tail.cat(s).push(head); }

  public Stack<Key> rev_cat(final Stack<Key> s) {
    return tail.rev_cat(s.push(head)); }

  public NStack<Key> isort() {
    return tail.isort().insert(head); }

  protected NStack<Key> insert(final Key k) {
    return head.compareTo(k) < 0 ?
           tail.insert(k).push(head)
         : push(k);
  }

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
