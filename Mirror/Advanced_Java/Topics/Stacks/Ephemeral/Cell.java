public class Cell<Key extends Comparable<Key>> {
  protected Key value;
  protected Cell<Key> next;

  public Cell (final Key key, final Cell<Key> cell) {
    value = key; next = cell; }

  public void print () {
    System.out.print(value + " "); 
    if (next != null) next.print(); 
  }

}
