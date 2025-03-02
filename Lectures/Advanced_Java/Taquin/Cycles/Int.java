public final class Int extends Node {

  private Node left, right, down, up;

  public Int (final Int root) {
    left = right = down = up = root; }

  public Int (final Int root, final char c) {
    final Ext cycle = new Ext();
    switch (c) {
    case 'l': left = down = up = root; right = cycle; break;
    case 'r': right = down = up = root; left = cycle; break;
    case 'd': left = right = down = root; up = cycle; break;
    case 'u': left = right = up = root; down = cycle; break;
    default : assert false;
    }
  }

  protected void insert (final Int root, final String cycle, 
                         final int index) throws Prefix {
    switch (cycle.charAt(index)) {
    case 'l': if (left == root)
                if (1+index == cycle.length())
                  left = new Ext();
                else { left = new Int(root,'l');
                       left.insert(root,cycle,index+1); }
              break;
    case 'r': if (right == root)
                if (1+index == cycle.length())
                  right = new Ext();
                else { right = new Int(root,'r');
                       right.insert(root,cycle,index+1); }
              break;
    case 'd': if (down == root)
                if (1+index == cycle.length())
                  down = new Ext();
                else { down = new Int(root,'d');
                       down.insert(root,cycle,index+1); }
              break;
    case 'u': if (up == root)
                if (1+index == cycle.length())
                  up = new Ext();
                else { up = new Int(root,'u');
                       up.insert(root,cycle,index+1); }
              break;
    default : assert false;
    }
  }

  public Node next (final char c) {
    switch (c) {
    case 'l': return left;
    case 'r': return right;
    case 'd': return down;
    case 'u': return up;
    default : throw new AssertionError();
    }
  }

}
