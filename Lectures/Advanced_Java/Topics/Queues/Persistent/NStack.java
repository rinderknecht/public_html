public final class NStack<Key> extends Stack<Key> {
  private final Key head;
  private final Stack<Key> tail;

  public NStack (final Key k, final Stack<Key> s) {
    head = k; tail = s; }

  public final boolean isEmpty () { return false; }

  public Pair<Key,Stack<Key>> pop () {
    return new Pair<Key,Stack<Key>>(head,tail);
  }

  public Stack<Key> rev_cat (final Stack<Key> s) {
    return tail.rev_cat(s.push(head)); }

  public Queue<Key> mkq (final Stack<Key> r) {
    return new NQueue<Key>(this,r);
  }

  public void print () {
    System.out.print(" " + head); tail.print(); 
  }

  public void rev_print () {
    tail.rev_print(); System.out.print(" " + head);
  }
}
