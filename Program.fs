open System

type GameOver =
    | Victory of String
    | Defeat of String
    | Draw

type Effect<'S> = {
    PreCondition: 'S -> bool;
    action: 'S -> Result<'S, GameOver>;
}

type Card<'S> = {
    Name: String;
    Effects: Effect<'S> list;
}

[<Literal>]
let InitialHandsNumber = 5;

type Player<'S> = {
    Name: String;
    Deck: Card<'S> list;
    Hands: Card<'S> list;
    BattleField: Card<'S> list;
    Graveyard: Card<'S> list;
} with
    member this.Draw n =
        match (n, this.Deck) with
        | 0, _ -> Ok this
        | _, [] -> Error (Defeat this.Name)
        | n, x :: xs ->
            { this with Deck = xs; Hands = x :: this.Hands}.Draw (n-1)

    static member Init name deck =
        let player = {
            Name = name;
            Deck = deck;
            Hands = [];
            BattleField = [];
            Graveyard = [];
        }
        let (Ok player) = player.Draw(InitialHandsNumber)
        player

type State = {
    Player1: Player<State>;
    Player2: Player<State>;
}

let player1Deck = [
    {
        Name = "A";
        Effects = [];
    };
    {
        Name = "A";
        Effects = [];
    };
    {
        Name = "A";
        Effects = [];
    };
    {
        Name = "B";
        Effects = [];
    };
    {
        Name = "B";
        Effects = [];
    };
    {
        Name = "C";
        Effects = [];
    };
    {
        Name = "C";
        Effects = [];
    }
];

let player2Deck = [
    {
        Name = "D";
        Effects = [];
    };
    {
        Name = "D";
        Effects = [];
    };
    {
        Name = "D";
        Effects = [];
    };
    {
        Name = "B";
        Effects = [];
    };
    {
        Name = "B";
        Effects = [];
    };
    {
        Name = "C";
        Effects = [];
    };
    {
        Name = "E";
        Effects = [];
    }
];


let rec mainLoop state =
    printfn "%s's turn!" state.Player1.Name
    match state.Player1.Draw 1 with
        | Error err -> err
        | Ok player1 ->
            let state = {state with Player1 = player1}
            printfn "%s's turn!" state.Player2.Name
            match state.Player2.Draw 1 with
            | Error err -> err
            | Ok player2 -> mainLoop {state with Player2 = player2}

[<EntryPoint>]
let main argv =
    let state = {
        Player1 = Player.Init "Player1" player1Deck;
        Player2 = Player.Init "Player2" player2Deck;
    }
    match mainLoop state with
    | Victory name -> printfn "Victory : %s" name
    | Defeat name -> printfn "Defeat: %s" name
    | Draw -> printfn "Draw!"
    0

