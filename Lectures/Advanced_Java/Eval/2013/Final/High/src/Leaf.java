public class Leaf<Key extends Comparable<Key>> extends LTree<Key> {
  protected final Key key;

  public Leaf (final Key k) { key = k; }

  public final Stack<Key> sort () { return new EStack<Key>(); }

  protected final Stack<Key> sort_aux (final EStack<Key> e) {
    return e; }
}
