package ai.acyclic.prover.commons;

public class JPeer {

    public static String hello() {
        return "hi from Java";
    }

    public static String greetFromScala() {
        return SPeer.hello();
    }
}
