public class calc {
  public int add(int a, int b) {
    return a + b;
  }
  public int sub(int a, int b) {
    return a - b;
  }
  public int mul(int a, int b) {
    return a * b;
  }
  public int div(int a, int b) {
    return a / b;
  }
  public static void main(String[] argv) {
    new calc().test();
  }
  public void test() {
    System.out.println(div(mul(add(10,20),sub(3,4)),2));
  }
}

