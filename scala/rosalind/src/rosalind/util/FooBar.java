package rosalind.util;

import java.util.HashMap;
import java.util.Map;

/**
 * Created by nikita on 11.12.14.
 */
public class FooBar {
    public static void main(String[] args){
        byte[] a1 = new byte[]{1, 2, 3};
        byte[] a2 = new byte[]{1, 2, 3};
        byte[] v = new byte[]{42};

        System.out.println(a1 == a2);

        Map<byte[], byte[]> foo = new HashMap<byte[], byte[]>();
        foo.put(a1, v);
        System.out.println(foo.containsKey(a1));
        System.out.println(foo.containsKey(a2));
        System.out.println(foo.get(a2));
    }
}
