public class Node<Key extends Comparable<Key>> extends LTree<Key>{
  protected final LTree<Key> left, right;

  public Node (final LTree<Key> l, final LTree<Key> r) {
    assert l != null && r != null;
    left = l; right = r; 
  }

  public Stack<Key> sort() {
    return this.sort_aux(new EStack<Key>()); }

  protected Stack<Key> sort_aux (final EStack<Key> e) {
    return left.sort_aux(e).merge(right.sort_aux(e)); }
}
