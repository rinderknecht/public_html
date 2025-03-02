public final class EStack<Key extends Comparable<Key>> 
       extends Stack<Key> {

  public final boolean isEmpty () { return true; }

  public final Stack<Key> cat(final Stack<Key> s) {
    return s; }

  public final Stack<Key> rev_cat(final Stack<Key> s) { 
    return s; }

  public final EStack<Key> isort() { return this; }

  protected final NStack<Key> insert(final Key k) {
    return push(k); }

  public final Stack<Key> merge(final Stack<Key> s) { return s; }

  protected final NStack<Key> merge_aux(final Key k,
                                        final Stack<Key> s,
                                        final NStack<Key> orig) {
    return orig; }

  public void print() { System.out.println(); }
}
