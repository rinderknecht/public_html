public final class NQueue<Key> extends Queue<Key> {
  protected final NStack<Key> front;
  protected final Stack<Key> rear;

  protected NQueue (final NStack<Key> f, final Stack<Key> r) {
    front = f; rear = r; }

  public NQueue<Key> enqueue (final Key k) {
    return new NQueue<Key>(front,rear.push(k)); }

  public Pair<Key,Queue<Key>> dequeue () {
    Pair<Key,Stack<Key>> p = front.pop();
    return new Pair<Key,Queue<Key>>(p.fst(), p.snd().mkq(rear));
  }

  public void print () { front.print(); rear.rev_print(); }
}
