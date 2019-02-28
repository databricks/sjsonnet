package sjsonnet;

public class Example {
    public void example(){
        sjsonnet.SjsonnetMain.main0(
            new String[]{"foo.jsonnet"},
            sjsonnet.SjsonnetMain.createParseCache(),
            System.in,
            System.out,
            System.err,
            os.package$.MODULE$.pwd(),
            scala.None$.empty()
        );
    }
}
