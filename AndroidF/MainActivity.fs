namespace AndroidF

open System

open Android.App
open Android.Content
open Android.OS
open Android.Runtime
open Android.Views
open Android.Widget
//[<Activity(Label = "@string/activity2")>]
type Activity1() =
    inherit ListActivity()

    let data = [|"one"; "two"; "three"; "four"; "five"|]

    override this.OnCreate (bundle) =
        base.OnCreate (bundle);
        let adapter = new ArrayAdapter (this, Resource_Id.textView4, data)
        this.ListAdapter <- adapter;

    override this.OnListItemClick (l, v, position, id) =
        base.OnListItemClick (l, v, position, id);
        (Toast.MakeText (this, data.[position], ToastLength.Short)).Show()
module prog = 
    open System
    let dic words = 
        let xs = 
            words
            |> Seq.map (fun (x:string) -> x.Split('#').[0])
            |> Seq.filter (String.forall (fun c -> Char.IsLower(c)))
            |> Seq.map (String.map (function 'ё' -> 'е' | x -> x))
            |> List.ofSeq
        xs |> Seq.groupBy (Seq.length)
        |> Map.ofSeq

    let possible count gen dic =
        match Map.tryFind count dic with
        | None -> Seq.empty
        | Some str ->
            let words = CombChars.word.extracts str |> CombChars.withoutStar.convert
            CombChars.withoutStar.possibleWords words gen |> Seq.map snd |> Seq.concat

    let generate (chars, count) = 
        let comb i = CombChars.comb.comb i chars
        comb count |> CombChars.withoutStar.convertWS

[<Activity (Label = "AndroidF", MainLauncher = true)>]
type MainActivity () =
    inherit Activity ()
    
    let mutable count:int = 1

    override this.OnCreate (bundle) =

        base.OnCreate (bundle)
        
        // Set our view from the "main" layout resource
        this.SetContentView (Resource_Layout.Main)
        // Get our button from the layout resource, and attach an event to it
        let button = this.FindViewById<Button>(Resource_Id.MyButton)
        //let lst = this.FindViewById<ListView>(Resource_Id.listView1)
        
        //let ar = new ArrayAdapter<string>(this, Resource_Id.textView4, [|"1"; "2"|])
        let txtview = this.FindViewById<TextView>(Resource_Id.textView4)
        //lst.Adapter <- ar
        //ar.GetItem(0)
        //let st = Java.Lang.Object.op_Implicit("sdf")
        (*
        let inline (!>) (x:^a) : ^b = ((^a or ^b) : (static member op_Implicit : ^a -> ^b) x)
        
        let f () = 
            let activity2 = new Intent (this, typeof<Activity1>);
            this.StartActivity activity2 *)
        //ar.Add (!> "kjf")
        //ar.Add st
        //ar.Add("kjf")
        //textView.SetText("new label in lstview", TextView.BufferType.Normal)
        //lst.AddView(textView)
        
        let dic = 
            let f () = 
                let pathDic = 
#if DEBUG
                    "dicmock.txt"
#else
                    "dic.txt"
#endif
                use ld = this.Assets.Open pathDic
                use st = new IO.StreamReader(ld, Text.Encoding.UTF8)
                let xs = [ while not <| st.EndOfStream do yield st.ReadLine() ]
                xs
            f() |> prog.dic

        let charsInput = this.FindViewById<EditText>(Resource_Id.editText2)
        let countInput = this.FindViewById<EditText>(Resource_Id.editText1)
        let msgbox (str:string) = let t = Toast.MakeText(this, str, ToastLength.Short ) in t.Show()
#if DEBUG
        charsInput.Text <- "абаклова"
        countInput.Text <- "4"
#endif
        let nl = System.Environment.NewLine
        button.Click.Add (fun _ -> 
            button.Enabled <- false
            txtview.Text <- ""
            let chars = charsInput.Text
            if chars = "" then msgbox "input chars empty"
            elif countInput.Text = "" then msgbox "input count empty"
            else
                let count = Int32.Parse countInput.Text
                if count > chars.Length then msgbox "count > chars"
                else
                    let gen = prog.generate(chars, count)
                    let words = prog.possible count gen dic
                    words |> Seq.iter ((+) nl >> txtview.Append)
            button.Enabled <- true
                (*
            button.Text <- sprintf "%d clicks!" count
            count <- count + 1
            txtview.Append(sprintf "\n%d clicks!" count)
            //if count % 20 = 0 then txtview.Text <- "count%20 =  0"
            
            //()
            //act.StartActivity()
            *)
        )