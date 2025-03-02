  public boolean mem(final Item i) {
      return false; }

  public int size() { return 0; }
  public int height () { return 0; }
  public Item max() throws EmptyBST { throw new EmptyBST(); }
  public Item max_aux(final Item parent) throws EmptyBST { return parent; }
