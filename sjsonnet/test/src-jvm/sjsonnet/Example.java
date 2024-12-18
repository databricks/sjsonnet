package sjsonnet;

import scala.collection.immutable.Map$;

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
            new sjsonnet.Std(Map$.MODULE$.empty()).Std()
        );
    }
}
