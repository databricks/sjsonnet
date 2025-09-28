package sjsonnet;

import scala.collection.immutable.Map$;

public class Example {
    public void example(){
        sjsonnet.SjsonnetMainBase.main0(
            new String[]{"foo.jsonnet"},
            new DefaultParseCache(),
            System.in,
            System.out,
            System.err,
            os.package$.MODULE$.pwd(),
            scala.None$.empty(),
            scala.None$.empty(),
            new sjsonnet.stdlib.StdLibModule(Map$.MODULE$.empty(), Map$.MODULE$.empty()).module()
        );
    }
}
