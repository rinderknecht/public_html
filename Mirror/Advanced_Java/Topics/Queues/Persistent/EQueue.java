public final class EQueue<Key> extends Queue<Key> {

  public NQueue<Key> enqueue (final Key k) {
    EStack<Key> empty = new EStack<Key>();
    return new NQueue<Key>(empty.push(k),empty);
  }

  public Pair<Key,Queue<Key>> dequeue () throws QEmpty {
    throw new QEmpty ();
  }

  public void print () { System.out.println(); }
}
