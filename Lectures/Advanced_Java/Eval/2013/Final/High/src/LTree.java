public abstract class LTree<Key extends Comparable<Key>> {
  public abstract Stack<Key> sort ();
  protected abstract Stack<Key> sort_aux (final EStack<Key> e);
}
