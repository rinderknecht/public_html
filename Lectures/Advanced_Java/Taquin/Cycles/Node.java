public abstract class Node {
  protected abstract void insert
    (final Int root, final String cycle, 
     final int index) throws Prefix;
  public abstract Node next (final char c) throws Cycle;
}
