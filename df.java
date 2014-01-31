import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.StringReader;

import static java.lang.Character.isAlphabetic;
import static java.lang.Character.isDigit;
import static java.lang.Character.isWhitespace;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Stack;
import java.util.TimeZone;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class df {
    // World's worst option mechanism
    private static boolean opt_d   = false;     // debug
    private static boolean opt_h   = false;     // help
    private static boolean opt_v   = false;     // validate
    private static boolean opt_dot = false;   // static input of "now"

    // World's worst one-letter recursive descent expression compiler
    // ... I know it looks like a compsci 101 C program
    // ... but to be fair, I want to keep everything in a single .class file
    private static StringBuffer      b = new StringBuffer();      // parse buffer
    private static int               l = 0;                       // b.length()
    private static int               p = 0;                       // index into b
    private static int               p_= 0;                       // previous index into b
    private static Object            n = null;                    // lookahead (next) token
    private static ArrayList<Object> r = new ArrayList<Object>(); // compilation results (RPN)
    private static long              now = new Date().getTime();
    private static String[]          tokens  = {"now",
                                                "milliseconds", "ms",
                                                "seconds",      "s",
                                                "minutes",      "m",
                                                "hours",        "h",
                                                "days",         "d"};
    // maintenance note: make sure symbols are longest-to-shortest if a shorter symbol
    //                   is a possible prefix of a longer one.  the list is matched in
    //                   simple as-is order.
    private static String[]          symbols  = {"+", "-", "*", "/", "(", ")", ".", ","};

    // report error and die, placing ^ at p_
    private static void e (String message) {
        System.err.println(message + ": " + b);
        char[] pad = new char[message.length()+2+p_+1];
        for (int i=0; i<pad.length-1; i++) pad[i]=' ';
        pad[pad.length-1]='^';
        System.err.println(pad);
        throw (new IllegalArgumentException());
    }

    // tokenizer: scan next token into n, update p, leave p_ behind for error reporting
    private static void k () {
        // skip whitespace and bail out at end of line
        while (p<l && isWhitespace(b.charAt(p))) p++;
        p_ = p;
        if (p >= l) {
            n = null;
            return;
        }
        // look for known reserved word tokens
        String token;
        int    mark;
        if (isAlphabetic(b.charAt(p))) {
            mark = p+1;
            while (mark<l && isAlphabetic(b.charAt(mark))) mark++;
            token = b.substring(p,mark);
            for (String t : tokens) {
                if (t.equalsIgnoreCase(token)) {
                    p = mark;
                    n = t;
                    return;
                }
            }
        }
        // look for symbols, which should be pre-sorted longest-to-shortest if
        // there are any prefix ambiguities (like "*" and "**", for example) since
        // the first match wins.
        for (String s : symbols) {
            int sl = s.length();
            if (p+sl<=l && b.substring(p,p+sl).equals(s)) {
                p += sl;
                n = s;
                return;
            }
        }
        // look for :<SimpleDateFormat>
        if (b.charAt(p)==':') {
            SimpleDateFormat f = f822; // default to [2]822 format if it's empty
            String fb;
            mark = p+1;
            while (mark<l && isWhitespace(b.charAt(mark))) mark++;
            if (mark<l && (b.charAt(mark)=='\'' || b.charAt(mark)=='"')) {
                // quote mode: format is in the quotes, \x is x
                StringBuffer format = new StringBuffer(l-mark);
                char quote = b.charAt(mark);
                int pquote = mark;
                mark++;
                while (mark<l && b.charAt(mark) != quote) {
                    if (b.charAt(mark)=='\\') {
                        mark++;
                        if (mark>=l) {
                            p_ = pquote;
                            e ("unmatched quoted string");
                        }
                    }
                    format.append(b.charAt(mark));
                    mark++;
                }
                if (mark>=l) {
                    p_ = pquote;
                    e ("unmatched quoted string");
                }
                mark++;
                fb = format.toString();
                p = mark;
            } else {
                // non-quote mode: chomp everything that remains (so much for , more...)
                fb = b.substring(p+1);
                p = l;
            }
            if (fb.length()>0) {
                try {
                    if (fb.equals("8601")) {
                        f = f8601;
                    } else if (fb.equals("822")) {
                        f = f822;
                    } else {
                        f = new SimpleDateFormat(fb);
                    }
                } catch (IllegalArgumentException e) {
                    p_ = p;
                    e ("invalid date format");
                }
            }
            f.setTimeZone(TimeZone.getTimeZone("GMT"));
            n = f;
            return;
        }
        // look for numbers
        if (isDigit(b.charAt(p))) {
            mark = p+1;
            while (mark<l && isDigit(b.charAt(mark))) mark++;
            try {
                Long v = Long.valueOf(b.substring(p,mark));
                p = mark;
                n = v;
                return;
            } catch (NumberFormatException e) {
                p_ = p;
                e ("invalid number");
            }
        }
        // nope
        p_ = p;
        e ("unrecognized token");
    }

    // string lookahead
    private static boolean is (String s) {
        return n != null &&
               n instanceof String &&
               ((String)n).equalsIgnoreCase(s);
    }

    // format suffix lookahead
    private static boolean isformat () {
        return (n != null &&
                (n instanceof SimpleDateFormat) ||
                 is("milliseconds") || is("ms") ||
                 is("seconds")      || is("s")  ||
                 is("minutes")      || is("m")  ||
                 is("hours")        || is("h")  ||
                 is("days")         || is("d"));
    }

    // compile a terminal value, possibly with unary -: v=[-] (number|.|now|(x))
    private static void v() {
        boolean minus = is("-");
        if (minus) k();
        if (is ("(")) {
            k();
            x();
            if (!is (")")) {
                e (") expected");
            }
            k();
        } else if (is(".") || n instanceof Long) {
            r.add(n);
            k();
        } else if (is("now")) {
            r.add(now);
            k();
        } else {
            e ("number (or . or now or parenthesis) expected");
        }
        if (minus) {
            r.add("U-");
        }
    }

    // compile a term: t=v {(*|/) v}
    private static void t() {
        v();
        while (is("*") || is("/")) {
            String op = (String) n;
            k();
            v();
            r.add(op);
        }
    }

    // compile an expression: x=t {(+|-) t}
    private static void x() {
        t();
        while (is("+") || is("-")) {
            String op = (String) n;
            k();
            t();
            r.add(op);
        }
    }

    // compile a command: c=[expr] [format]
    private static void c() {
        if (n!=null && !(isformat()||is(","))) {
            x();
        } else {
            r.add(".");
        }
        if (isformat()) {
            if (n instanceof SimpleDateFormat) {
                r.add(n);
            } else if (n instanceof String) {
                switch ((String)n) {
                case "ms":
                case "milliseconds":
                    break;
                case "s":
                case "seconds":
                    r.add(new Long(1000));
                    r.add("/");
                    break;
                case "m":
                case "minutes":
                    r.add(new Long(1000*60));
                    r.add("/");
                    break;
                case "h":
                case "hours":
                    r.add(new Long(1000*60*60));
                    r.add("/");
                    break;
                case "d":
                case "days":
                    r.add(new Long(1000*60*60*24));
                    r.add("/");
                    break;
                default:
                    e ("valid format option expected");
                }
                r.add("p");
            } else {
                e ("valid format option expected");
            }
            k();
        } else {
            r.add("p");
        }
    }

    /*
     * Grammar:
     *     command: c = [expr] [format]
     *     expr:    x = term { (+|-) term}
     *     term:    t = value { (*|/) value}
     *     value:   v = [-] (. | now | number | (expr) )
     */
    private static void compile(String[] args) {
        // slurp args into b and set l and load n
        boolean opt_dash = false;
        for (String arg : args) {
            if (!opt_dash && arg.equals("--")) {
                opt_dash = true;
            } else if (!opt_dash && arg.equals("-d")) {
                opt_d = true;
            } else if (!opt_dash && arg.equals("-h")) {
                opt_h = true;
            } else if (!opt_dash && arg.equals("-v")) {
                opt_v = true;
            } else if (!opt_dash && arg.equals("-.")) {
                opt_dot = true;
            } else {
                if (b.length()>0) b.append(' ');
                b.append(arg);
            }
        }
        l = b.length();
        p_ = p = 0;
        k();

        // compile command stream
        c();
        while (is(",")) {
            k();
            c();
            r.add(",");
        }

        // that should be it
        if (n!=null) {
            e ("unrecognized extra stuff");
        }
        
        // debug output
        if (opt_d) {
            for (Object o : r) {
                System.err.println(o);
            }
        }
        
        // help output
        if (opt_h) {
            System.out.println("usage:      java df.class [options] [filter]");
            System.out.println("options:    -h  help  display this message");
            System.out.println("            -v  validate JSON attribute syntax");
            System.out.println("            -.  use \"now\" as the input stream");
            System.out.println("            -d  debug display compiled format stack");
            System.out.println("filter:     [expression][format]{,...}");
            System.out.println("expression: typical expression using +-*/ and ()");
            System.out.println("            expression terms can include:");
            System.out.println("            - numbers");
            System.out.println("            - now, meaning the current time in epoch milliseconds");
            System.out.println("            - ., meaning the input filter parsed time in epoch milliseconds");
            System.out.println("format:     :SimpleDateFormat to format as a string");
            System.out.println("            ms or milliseconds to print the number as is");
            System.out.println("            s or seconds to display as epoch seconds");
            System.out.println("            m or minutes to display as epoch minutes");
            System.out.println("            h or hours to display as epoch hours");
            System.out.println("            d or days to display as epoch days");
            System.out.println("input:      stdin is copied to stdout, replacing date attributes with formatted filter");
            System.out.println("            the pattern \"attribute\": \"value\" (or :value for numbers) is matched");
            System.out.println("            dates formatted eg: \"Tue, 01 Oct 2013 14:38:11 +0000\" are replaced");
            System.out.println("            dates formatted as long milliseconds (12-13 digits) are also replaced");
            System.out.println("            note that whitespace around the : is not significant");
            System.out.println("default:    default expression is ., format is ms");
            System.out.println("            .:EEE, dd MMM yyyy HH:mm:ss Z is passthrough");
            throw (new IllegalArgumentException()); // -h means don't do anything
        }
    }

    // evaluator of RPN "program" in r.  Assumed to be balanced/valid RPN.
    private static String eval (long dot) {
        Stack<Long>   stack = new Stack<Long>();
        long          temp;
        String        stemp;
        Stack<String> result = new Stack<String>();
        for (Object op : r) {
            if (op instanceof Long) {
                stack.push((Long)op);
            } else if (op instanceof String) {
                switch((String)op) {
                case "+":
                    stack.push(stack.pop()+stack.pop());
                    break;
                case "-":
                    temp = stack.pop();
                    stack.push(stack.pop()-temp);
                    break;
                case "*":
                    stack.push(stack.pop()*stack.pop());
                    break;
                case "/":
                    temp = stack.pop();
                    stack.push(stack.pop()/temp);
                    break;
                case "U-":
                    stack.push(-stack.pop());
                    break;
                case ".":
                    stack.push(dot);
                    break;
                case "p":
                    result.push(Long.toString(stack.pop()));
                    break;
                case ",":
                    stemp = result.pop();
                    result.push(result.pop()+","+stemp);
                    break;
                }
            } else if (op instanceof SimpleDateFormat) {
                result.push(((SimpleDateFormat)op).format(new Date(stack.pop())));
            }
        }

        String check = result.pop();
        if (!check.matches("\\d+")) { // might need an option for this someday
            check = '"'+check+'"';
        }
        return check;
    }

    // main filter program
    static final SimpleDateFormat f822  = new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss Z");
    static final SimpleDateFormat f8601 = new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SXXX");
    static final String           sattr = "(\"(\\w+)\"\\s*:\\s*)";
    static final String           s822  = "[A-Z][a-z]{2}, \\d{2} [A-Z][a-z]{2} \\d{4} \\d{2}:\\d{2}:\\d{2} [-+]\\d{4}";
    static final String           s8601 = "(-)?(\\d{4}-\\d{2}-\\d{2}T\\d{2}:\\d{2}:\\d{2})(\\.\\d+)?((?:[+-]\\d{2}:\\d{2})|Z)?";
    static final String           slong = "\\d{12,13}";
    static final String           sany  = "\"?("+s822+"|"+s8601+"|"+slong+")\"?";
    static       Pattern          pany;
    static final Pattern          p8601 = Pattern.compile(s8601);

    private static String convert (String expr) {
        try {
            if (expr.contains(",")) {
                Date date = f822.parse(expr);
                return eval(date.getTime());
            } else if (expr.contains("-")) {
                Matcher m8601=p8601.matcher(expr); // groups: 1=-? 2=dateTtime 3=.mills 4=tz
                m8601.matches();  // yes, it does
                String mills = m8601.group(3);
                String tzone = m8601.group(4);
                if (mills==null || mills.length()==0) mills=".0";
                if (tzone==null || tzone.length()==0) tzone="Z";
                Date date = f8601.parse(m8601.group(2)+mills+tzone);
                return eval(date.getTime());
            } else {
                return eval(Long.valueOf(expr));
            }
        } catch (Exception e) {
            return expr;
        }
    }

    /**
     * @param args
     */
    public static void main(String[] args) {
        // compile or bust
        try {
            compile(args);
        } catch (IllegalArgumentException e) {
            return;
        }
        // compile patterns based on -v
        opt_v = opt_v && !opt_dot;  // -v can't work with -.
        if (opt_v) {
            pany = Pattern.compile(sattr+sany);
        } else {
            pany = Pattern.compile("(())"+sany);  // maintain capture group numbers
        }
        // filter
        try (BufferedReader in =
               opt_dot ? new BufferedReader(new StringReader(Long.toString(now)))
                       : new BufferedReader(new InputStreamReader(System.in))) {
            String line;
            while ((line = in.readLine())!=null) {
                StringBuffer sb = new StringBuffer();
                Matcher many = pany.matcher(line);
                while (many.find()) {
                    // many.group(2) is the attribute name
                    // can enforce known attrs with -v someday
                    many.appendReplacement(sb, many.group(1)+convert(many.group(3)));
                }
                many.appendTail(sb);
                System.out.println(sb.toString());
            }
        } catch (Exception e) {
            System.err.println("error: "+e);
        }
    }
}
