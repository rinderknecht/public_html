public class Pair<Fst,Snd> {
  protected final Fst fst;
  protected final Snd snd;

  public Pair(final Fst f, final Snd s) { fst = f; snd = s; }

  public Fst fst() { return fst; }
  public Snd snd() { return snd; }
}
