import com.ericsson.otp.erlang.*;
import processing.core.*;
import java.awt.BorderLayout;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.JFrame;

public class ErlProc
{
  OtpNode node;
  OtpMbox mbox;

  OtpErlangPid erlPid;
  String erlNodeName;
  Embedded embed;

  ProcessingFrame frame;

  public ErlProc()
  {
  }
  
  public void go()
  {
    String str;
    OtpMsg message;
    OtpErlangTuple tuple;
    OtpErlangAtom command;
    try
    {
      OtpErlangObject [] items = new OtpErlangObject[3];
      
      /*
        The Erlang node name comes via the port
      */
      BufferedReader in = new BufferedReader(new InputStreamReader(System.in));
      erlNodeName = in.readLine();
      
      /* Create a new node and mailbox */
      node = new OtpNode("processing");
      mbox = node.createMbox("processing");
      
      /*
        Send the message to the Erlang by name;
        it will have my Pid and node name
      */
      items[0] = new OtpErlangAtom("kabuki");
      items[1] = mbox.self();
      items[2] = new OtpErlangAtom(node.node());
      mbox.send("erlproc", erlNodeName, new OtpErlangTuple(items));
      
      /* Erlang sends me a message from which I can get its Pid */
      message = mbox.receiveMsg();
      tuple = (OtpErlangTuple) message.getMsg();
      command = (OtpErlangAtom) tuple.elementAt(0);
 
      /* And I send back a message to let Erlang know I've finished */
      if (command.atomValue().equals("kabuki"))
      {
        erlPid = message.getSenderPid();
        mbox.send(erlPid, new OtpErlangAtom("kabuki_complete"));
        processingLoop();
      }
      else
      {
        System.err.println("Java: Unknown command " + command.atomValue()
          + "\r");
      }
    }
    catch (Exception e)
    {
      System.err.println("ERR" + e);
      e.printStackTrace();
    }
  }
 
  private float[] getFloatList(OtpErlangObject obj)
  {
    float[] result = null;
    if (OtpErlangString.class.isInstance(obj))
    {
      String str = ((OtpErlangString) obj).stringValue();
      int[] codePoints = OtpErlangString.stringToCodePoints(str);
      result = new float[str.length()];
      for (int i = 0; i < str.length(); i++)
      {
        result[i] = codePoints[i];
      }
    }
    else if (OtpErlangList.class.isInstance(obj))
    {
      OtpErlangList list = (OtpErlangList) obj;
      int nItems = list.arity();
      result = new float[nItems];
      for (int i = 0; i < nItems; i++)
      {
        try
        {
          if (OtpErlangDouble.class.isInstance(list.elementAt(i)))
          {
            result[i] = ((OtpErlangDouble) list.elementAt(i)).floatValue();
          }
          else if (OtpErlangLong.class.isInstance(list.elementAt(i)))
          {
            result[i] = ((OtpErlangLong) list.elementAt(i)).intValue();
          }
          else
          {
            result[i] = 0.0F;
          }
        }
        catch (Exception e)
        {
          e.printStackTrace(System.err);
          result[i] = 0.0F;
        }
      }
    }
    return result;
  }
  
  private void processingLoop()
  {
    boolean exit = false;
    OtpMsg message;
    OtpErlangTuple tuple;
    
    String command;
    float[] coords;
    while (!exit)
    {
      try {
        message = mbox.receiveMsg();
        tuple = (OtpErlangTuple) message.getMsg();
        command = ((OtpErlangAtom) tuple.elementAt(0)).atomValue();
        
        System.err.println("Java gets command: " + command + "\r");
        if (command.equals("sketch"))
        {
          float[] dimensions = new float[2];
          int width;
          int height;

          dimensions = getFloatList(tuple.elementAt(1));
          width = (int) dimensions[0];
          height = (int) dimensions[1];

          frame = new ProcessingFrame(width, height);
          embed = frame.embed;
          frame.setResizable(true);
          frame.setSize(width, height);
          frame.setVisible(true);
        }
        else if (command.equals("redraw"))
        {
          embed.redraw();
          embed.noLoop();
        }
        else if (command.equals("rect"))
        {
          coords = getFloatList(tuple.elementAt(1));
          if (coords.length >= 4)
          {
            embed.rect(coords[0], coords[1], coords[2], coords[3]);
          }
        }
        else if (command.equals("ellipse"))
        {
          coords = getFloatList(tuple.elementAt(1));
          embed.ellipse(coords[0], coords[1], coords[2], coords[3]);
        }
        else if (command.equals("fill"))
        {
          doFill(tuple.elementAt(1));
        }
        else if (command.equals("noFill"))
        {
          embed.noFill();
        }
        else if (command.equals("background"))
        {
          doBackground(tuple.elementAt(1));
        }
        else if (command.equals("stroke"))
        {
          doStroke(tuple.elementAt(1));
        }
        else if (command.equals("nostroke"))
        {
          embed.noStroke();
        }
        else if (command.equals("line"))
        {
          coords = getFloatList(tuple.elementAt(1));
          if (coords.length >= 4)
          {
            embed.line(coords[0], coords[1], coords[2], coords[3]);
          }
        }
        else if (command.equals("triangle"))
        {
          coords = getFloatList(tuple.elementAt(1));
          if (coords.length >= 6)
          {
            embed.triangle(coords[0], coords[1], coords[2], coords[3],
              coords[4], coords[5]);
          }
        }
        else if (command.equals("quad"))
        {
          coords = getFloatList(tuple.elementAt(1));
          if (coords.length >= 8)
          {
            embed.quad(coords[0], coords[1], coords[2], coords[3],
              coords[4], coords[5], coords[6], coords[7]);
          }
        }
        else if (command.equals("mouse"))
        {
          doMouse();
        }
        else if (command.equals("pmouse"))
        {
          doPMouse();
        }
      }
      catch (OtpErlangExit e)
      {
        e.printStackTrace(System.err);
        System.exit(0);
      }
      catch (Exception e)
      {
        e.printStackTrace(System.err);
      }
    }
  }

  private void doFill(OtpErlangObject params)
  {
    float[] values = getFloatList(params);
    switch (values.length)
    {
      case 1: embed.fill(values[0]); break;
      case 2: embed.fill(values[0], values[1]); break;
      case 3: embed.fill(values[0], values[1], values[2]); break;
      case 4: embed.fill(values[0], values[1], values[2], values[3]); break;
      default: break;
    }
  }

  private void doBackground(OtpErlangObject params)
  {
    float[] values = getFloatList(params);
    switch (values.length)
    {
      case 1: embed.background(values[0]); break;
      case 2: embed.background(values[0], values[1]); break;
      case 3: embed.background(values[0], values[1], values[2]); break;
      case 4: embed.background(values[0], values[1], values[2], values[3]); break;
      default: break;
    }
  }

  private void doStroke(OtpErlangObject params)
  {
    float[] values = getFloatList(params);
    switch (values.length)
    {
      case 1: embed.stroke(values[0]); break;
      case 2: embed.stroke(values[0], values[1]); break;
      case 3: embed.stroke(values[0], values[1], values[2]); break;
      case 4: embed.stroke(values[0], values[1], values[2], values[3]); break;
      default: break;
    }
  }

  private void doMouse()
  {
    OtpErlangObject[] coords = new OtpErlangObject[2];
    OtpErlangObject[] tuple = new OtpErlangObject[2];
    coords[0] = new OtpErlangFloat(embed.mouseX);
    coords[1] = new OtpErlangFloat(embed.mouseY);
    tuple[0] = new OtpErlangAtom("mouse");
    tuple[1] = new OtpErlangList(coords);
    mbox.send(erlPid, new OtpErlangTuple(tuple));
  }

  private void doPMouse()
  {
    OtpErlangObject[] coords = new OtpErlangObject[2];
    OtpErlangObject[] tuple = new OtpErlangObject[2];
    coords[0] = new OtpErlangFloat(embed.mouseX);
    coords[1] = new OtpErlangFloat(embed.mouseY);
    tuple[0] = new OtpErlangAtom("pmouse");
    tuple[1] = new OtpErlangList(coords);
    mbox.send(erlPid, new OtpErlangTuple(tuple));
  }

  public static void main(String [] args)
  {
    ErlProc app = new ErlProc();
    app.go();    
  }
}

class ProcessingFrame extends JFrame {

  public Embedded embed;
    
  public ProcessingFrame(int width, int height) {
    super("Processing from Erlang");
    this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
    setLayout(new BorderLayout());
         
    Embedded embed = new Embedded(width, height);
    add(embed, BorderLayout.CENTER);
    this.embed = embed;

    // important to call this whenever embedding a PApplet.
    // It ensures that the animation thread is started and
    // that other internal variables are properly set.
    embed.init();     
   }
 }

class Embedded extends PApplet {
  
  int initialWidth;
  int initialHeight;
  
  public Embedded(int width, int height)
  {
    super();
    initialWidth = width;
    initialHeight = height;
  }
  
  public void setup() {
    // original setup code here ...
    size(initialWidth, initialHeight);
    background(255);

    // prevent thread from starving everything else
    noLoop();
  }

  public void draw() {
      // drawing code goes here
  }
}
  
