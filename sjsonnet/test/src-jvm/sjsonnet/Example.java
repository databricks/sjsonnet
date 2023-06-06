package sjsonnet;

public class Example {
    public void example(){
        sjsonnet.SjsonnetMain.main0(
            new String[]{"foo.jsonnet"},
            new DefaultParseCache(),
            System.in,
            System.out,
            System.err,
            os.package$.MODULE$.pwd(),
            scala.None$.empty(),
            scala.None$.empty(),
            new sjsonnet.Std().Std()
        );
    }
}
