public abstract class Queue<Key> {
  public abstract NQueue<Key> enqueue (final Key k);
  public abstract Pair<Key,Queue<Key>> dequeue () throws QEmpty;
  public abstract void print ();
}
