public class Zad3 {
  public static int fibRec(int n) {
    if (n < 2) {
      return n;
    } else {
      return fibRec(n - 1) + fibRec(n - 2);
    }
  }
  public static int fibIter(int n) {
    if (n == 0) {
      return 0;
    } else {
      int a = 0;
      int b = 1;
      while (n > 1) {
        int c = a;
        a = b;
        b = b + c;
        --n;
      }
      return b;
    }
  }
  public static void main(String[] args) {
    System.out.println(fibRec(8));
    System.out.println(fibIter(8));
  }
}
