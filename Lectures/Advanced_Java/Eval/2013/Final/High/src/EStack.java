public final class EStack<Key extends Comparable<Key>> 
       extends Stack<Key> {

  public final Stack<Key> merge(final Stack<Key> s) { return s; }

  protected final NStack<Key> merge_aux(final Key k,
                                        final Stack<Key> s,
                                        final NStack<Key> orig) {
    return orig; }

  public void print() { System.out.println(); }
}
