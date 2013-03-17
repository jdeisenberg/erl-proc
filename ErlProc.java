/*
  Interface Processing's core functions to Erlang
  Copyright 2013 by J. David Eisenberg
  Licensed under LGPL version 2.1 (same as Processing's core)
*/
import com.ericsson.otp.erlang.*;
import processing.core.*;
import java.awt.BorderLayout;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import javax.swing.JFrame;

public class ErlProc
{
  OtpSelf self;
  OtpConnection connection;

  OtpErlangPid erlPid;
  String erlNodeName;
  
  String erlFunctionName;

  ProcessingFrame frame;
  Embedded embed;

  public ErlProc()
  {
  }
  
  /**
   * Read a line from standard input. Output from Erlang may not be
   * buffered, so I do not want to make a BufferedReader based on System.in.
   */
  private String getLine()
  {
    byte[] buf = new byte[256];
    int ch = 0;
    int i = 0;
    try {
      ch = System.in.read();
      while (ch > 0 && ch != '\n' && i < 256)
      {
        if (ch >= 32) // skip control characters
        {
          buf[i++] = (byte) ch;
        }
        ch = System.in.read();
      }
    }
    catch (Exception e)
    {
      i = 0; // bail; return null string
    }
    return new String(buf, 0, i);
  }

  /**
   * Establish a connection with an Erlang node. Start by receiving
   * the Erlang node's name via the port.
   */
  public void go()
  {
    String str;
    OtpPeer erlangPeer;
    OtpMsg message;
    OtpErlangTuple tuple;
    OtpErlangAtom command;
    OtpErlangList dimensionList;
    float [] dimensions;
    
    try
    {
      OtpErlangObject [] items = new OtpErlangObject[3];
      OtpErlangObject obj;
      
      /*
        The Erlang node name comes via the port
      */
      erlNodeName = getLine();
      
      /* Create a connection to Erlang */
      self = new OtpSelf("java_process");
      erlangPeer = new OtpPeer(erlNodeName);
      connection = self.connect(erlangPeer);
      // System.err.println("Connected to " + erlNodeName 
      //  + " " + connection + "\r");
      
      /*
        Send a message to the Erlang process by name;
        it will have my Pid and node name in it.
      */
      // System.err.println("Java's pid is " + connection.self().pid() + "\r");
      items[0] = new OtpErlangAtom("kabuki");
      items[1] = connection.self().pid();
      items[2] = new OtpErlangString("java_process");
      connection.send("hub", new OtpErlangTuple(items));
      
      /*
        Erlang sends me a message from which I can get its Pid
        and the sketch dimensions
      */
      message = connection.receiveMsg();
      obj = message.getMsg();
      // System.err.println("Java gets message " + obj + "\r");
      tuple = (OtpErlangTuple) obj;
      command = (OtpErlangAtom) tuple.elementAt(0);
 
      /* And I send back a message to let Erlang know handshaking is finished */
      if (command.atomValue().equals("kabuki"))
      {
        int width;
        int height;
        
        erlPid = (OtpErlangPid) tuple.elementAt(1);
        dimensions = getFloatList(tuple.elementAt(2), 2);
        width = (int) dimensions[0];
        height = (int) dimensions[1];
        
        // System.err.println("Java gets Erlang's Pid: " + erlPid +
        //  " Dimensions: " + dimensionList  + "\r");
        connection.send(erlPid, new OtpErlangAtom("kabuki_complete"));
        
        // open the frame, and start the embedded PApplet
        frame = new ProcessingFrame(connection, width, height);
        embed = frame.embed;
        frame.setResizable(true);
        frame.setSize(width + 20, height + 20);
        frame.setVisible(true);
        embed.init();
      }
      else
      {
        // System.err.println("Java gets unknown command " + command.atomValue()
        //  + "\r");
      }
    }
    catch (Exception e)
    {
      // System.err.println("ERR" + e);
      e.printStackTrace();
    }
  }
  
  /**
   * Retrieve a list of floating point numbers from an Erlang list.
   */
  private float[] getFloatList(OtpErlangObject obj, int maxItems)
  {
    float[] result = null;
    
    /*
      Erlang represents a list of integers < 255 as a string.
      If I get a string, I have to convert each of those integers
      to a float.
    */
    if (OtpErlangString.class.isInstance(obj))
    {
      String str = ((OtpErlangString) obj).stringValue();
      int[] codePoints = OtpErlangString.stringToCodePoints(str);
      result = new float[str.length()];
      for (int i = 0; i < Math.min(maxItems, str.length()); i++)
      {
        result[i] = codePoints[i];
      }
    }
    else if (OtpErlangList.class.isInstance(obj)) // normal Erlang list
    {
      OtpErlangList list = (OtpErlangList) obj;
      int nItems = Math.min(maxItems, list.arity());
      result = new float[nItems];
      /*
        Coerce doubles and integers into floats. An OtpErlangFloat is
        a subclass of OtpErlangDouble, and an OtpErlangInt is a subclass of
        OtpErlangLong, so that's all I have to check for.
      */
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
            result[i] = 0.0F; // if I can't figure out what it is, bail out.
          }
        }
        catch (Exception e)
        {
          e.printStackTrace(System.err);
          result[i] = 0.0F; // likewise if coercion fails
        }
      }
    }
    return result;
  }
  
  /**
   * Accept messages from Erlang to draw to the sketch. This is where
   * all the work really happens.
   */
  private void executeDrawingCommands(OtpErlangList drawingList)
  {
    boolean exit = false;
    OtpErlangTuple tuple;
    
    String command;
    float[] coords;
    int nItems = drawingList.arity();
    
    for (int i = nItems -1 ; i >= 0; i--)
    {
      tuple = (OtpErlangTuple) drawingList.elementAt(i);
      command = ((OtpErlangAtom) tuple.elementAt(0)).atomValue();
        
      // System.err.println("Executing: " + command + "\r");
      if (command.equals("redraw"))
      {
        embed.redraw();
        embed.noLoop();
      }
      else if (command.equals("noLoop"))
      {
        embed.noLoop();
      }
      else if (command.equals("fill"))
      {
        doFill(tuple.elementAt(1));
      }
      else if (command.equals("noFill"))
      {
        embed.noFill();
      }
      else if (command.equals("stroke"))
      {
        doStroke(tuple.elementAt(1));
      }
      else if (command.equals("nostroke"))
      {
        embed.noStroke();
      }
      else if (command.equals("background"))
      {
        doBackground(tuple.elementAt(1));
      }
      else if (command.equals("line"))
      {
        coords = getFloatList(tuple.elementAt(1), 4);
        if (coords.length == 4)
        {
          embed.line(coords[0], coords[1], coords[2], coords[3]);
        }
      }
      else if (command.equals("rect"))
      {
        coords = getFloatList(tuple.elementAt(1), 4);
        if (coords.length == 4)
        {
          embed.rect(coords[0], coords[1], coords[2], coords[3]);
        }
      }
      else if (command.equals("ellipse"))
      {
        coords = getFloatList(tuple.elementAt(1), 4);
        if (coords.length == 4)
        {
          embed.ellipse(coords[0], coords[1], coords[2], coords[3]);
        }
      }
      else if (command.equals("triangle"))
      {
        coords = getFloatList(tuple.elementAt(1), 6);
        if (coords.length == 6)
        {
          embed.triangle(coords[0], coords[1], coords[2], coords[3],
            coords[4], coords[5]);
        }
      }
      else if (command.equals("quad"))
      {
        coords = getFloatList(tuple.elementAt(1), 8);
        if (coords.length == 8)
        {
          embed.quad(coords[0], coords[1], coords[2], coords[3],
            coords[4], coords[5], coords[6], coords[7]);
        }
      }
    }
  }

  private void doFill(OtpErlangObject params)
  {
    float[] values = getFloatList(params, 4);
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
    float[] values = getFloatList(params, 4);
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
    float[] values = getFloatList(params, 4);
    switch (values.length)
    {
      case 1: embed.stroke(values[0]); break;
      case 2: embed.stroke(values[0], values[1]); break;
      case 3: embed.stroke(values[0], values[1], values[2]); break;
      case 4: embed.stroke(values[0], values[1], values[2], values[3]); break;
      default: break;
    }
  }

  class Embedded extends PApplet {
    OtpErlangObject obj;
    OtpErlangList drawingList;
    OtpConnection connection;
    int width;
    int height;
    
    public Embedded(OtpConnection connection, int width, int height)
    {
      super();
      this.connection = connection;
      this.width = width;
      this.height = height;
    }
    
    private OtpErlangTuple createTuple(String atom, int value)
    {
      OtpErlangObject[] elements = new OtpErlangObject[2];
      elements[0] = new OtpErlangAtom(atom);
      elements[1] = new OtpErlangFloat(value);
      return new OtpErlangTuple(elements);
    }

    private OtpErlangTuple getEnvironment()
    {
      OtpErlangTuple [] items = new OtpErlangTuple[8];
      OtpErlangObject [] elements = new OtpErlangObject[2];
      OtpErlangTuple result;
      
      items[0] = createTuple("mouseX", this.mouseX);
      items[1] = createTuple("mouseY", this.mouseY);
      items[2] = createTuple("pmouseX", this.pmouseX);
      items[3] = createTuple("pmouseY", this.pmouseY);
      
      elements[0] = new OtpErlangAtom("mousePressed");
      elements[1] = new OtpErlangBoolean(this.mousePressed);
      items[4] = new OtpErlangTuple(elements);
      
      elements[0] = new OtpErlangAtom("mouseButton");
      if (this.mouseButton == PApplet.LEFT)
      {
        elements[1] = new OtpErlangAtom("left");
      }
      else if (this.mouseButton == PApplet.RIGHT)
      {
        elements[1] = new OtpErlangAtom("right");
      }
      else if (this.mouseButton == PApplet.CENTER)
      {
        elements[1] = new OtpErlangAtom("center");
      }
      else
      {
        elements[1] = new OtpErlangAtom("none");
      }
      items[5] = new OtpErlangTuple(elements);

      items[6] = createTuple("width", this.width);
      items[7] = createTuple("height", this.height);
      
      elements[0] = new OtpErlangAtom("environment");
      elements[1] = new OtpErlangList(items);
      result = new OtpErlangTuple(elements);
      return(result);
    }

    public void setup()
    {
      OtpErlangAtom atom;
      OtpErlangTuple tuple;
      
      size(width, height);
      try
      {
        // System.err.println("Java is sending environment\r");
        connection.send(erlPid, getEnvironment());
        atom = (OtpErlangAtom) connection.receiveMsg().getMsg();
        if (atom.atomValue().equals("ok"))
        {
          connection.send(erlPid, new OtpErlangAtom("setup"));
          tuple  = (OtpErlangTuple) connection.receiveMsg().getMsg();
          // System.err.println("Java told: " +
          //  ((OtpErlangAtom) tuple.elementAt(0)).atomValue() + "\r");
          executeDrawingCommands((OtpErlangList) tuple.elementAt(1));
        }
      }
      catch (Exception e)
      {
        // System.err.println("Cannot setup.\r");
        e.printStackTrace(System.err);
        System.exit(1);
      }
    }
  
    public void draw()
    {
      OtpErlangAtom atom;
      OtpErlangTuple tuple;
      
      try
      {
        connection.send(erlPid, getEnvironment());
        atom = (OtpErlangAtom) connection.receiveMsg().getMsg();
        if (atom.atomValue().equals("ok"))
        {
          connection.send(erlPid, new OtpErlangAtom("draw"));
          tuple  = (OtpErlangTuple) connection.receiveMsg().getMsg();
          // System.err.println("Java told: " +
          //  ((OtpErlangAtom) tuple.elementAt(0)).atomValue() + "\r");
          executeDrawingCommands((OtpErlangList) tuple.elementAt(1));
        }
      }
      catch (Exception e)
      {
        // System.err.println("Cannot draw.\r");
        e.printStackTrace(System.err);
        System.exit(1);
      }
    }
  }

  class ProcessingFrame extends JFrame {
  
    public Embedded embed;
      
    public ProcessingFrame(OtpConnection connection, int width, int height) {
      super("Processing from Erlang");
      this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
      setLayout(new BorderLayout());
           
      Embedded embed = new Embedded(connection, width, height);
      add(embed, BorderLayout.CENTER);
      this.embed = embed;
    }
  }

  public static void main(String [] args)
  {
    ErlProc app = new ErlProc();
    app.go();    
  }

}
