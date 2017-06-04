open HtmlAgilityPack
open System.Net
open System.Text.RegularExpressions

type Link = {
    Provider : string
    Url: string
}

type ResourceType =
    | Show
    | Season
    | Episode

let parseHref (a:HtmlNode) =
    a.Attributes
    |> Seq.cast<HtmlAttribute>
    |> Seq.tryFind (fun a -> a.Name = "href")

// TODO: use in parsing video
let parseVideoLink (a:HtmlNode) =
    
    let phrasesToRemove = ["&nbsp;"; "\n"; "\r"; "Broken"]
    let provider = List.fold (fun (acc:string) phrase -> acc.Replace(phrase, "").Trim()) a.InnerText phrasesToRemove

    let href = parseHref a
    let validNode = provider <> "" && parseHref a |> Option.isSome

    match validNode with 
    | true -> Some { Provider = provider; Url = (Option.get href).Value }
    | false -> None
    
let parseLinks html xpath =
    let document = new HtmlDocument()
    document.LoadHtml(html)

    document.DocumentNode.SelectNodes(xpath)
    |> Seq.cast<HtmlNode>
    |> Seq.map parseHref
    |> Seq.filter Option.isSome
    |> Seq.map Option.get
    |> Seq.toList

let parseLinksValues (hrefs:HtmlAttribute list) =
    hrefs |> List.map (fun href -> href.Value)
    
let trimBaseUrl (baseUrl:string) (url:string) =
    url.Replace(baseUrl, "")
    
let classifyResource resource = 
   match resource with
   // /episode/the-simpsons-season-28-episode-22/
   | resource when Regex.IsMatch(resource, "^episode/.+-season-[0-9]+-episode[0-9]+/$") -> Some ResourceType.Episode
   // /free/archer-season-1/
   | resource when Regex.IsMatch(resource, "^/free/.+-season-[0-9]+/$") -> Some ResourceType.Season
   // /free/archer/
   | resource when Regex.IsMatch(resource, "^/free/.+/$") -> Some ResourceType.Show
   | _ -> None

let downloadHtml (url:string) =
    use webClient = new WebClient()
    try
        Some(webClient.DownloadString(url))         
    with
        | _ -> None

let getEpisode seasonIndex episodeIndex =
    let url = sprintf "http://project-free-tv.ch/episode/scrubs-season-%d-episode-%d" seasonIndex episodeIndex
    let html = downloadHtml url
    match html with
       | Some(html) -> Some(parseLinks html "div[@id=\"mybox\"]/table/tbody/td/a")
       | None -> None

let getSearch baseUrl (searchText:string) =
    let url = baseUrl + sprintf "/search-tvshows/?free=%s" (System.Web.HttpUtility.UrlEncode searchText)
    let html = downloadHtml url
    match html with
       | Some(html) -> parseLinks html "//div[@id=\"content_box\"]/table/tbody/tr/th/div/a[1]" |> parseLinksValues |> List.map (trimBaseUrl baseUrl)
       | None -> []

let getShowSeasons baseUrl url =
    let html = downloadHtml (baseUrl + url)
    match html with
    | Some(html) -> parseLinks html "//div[@id=\"content_box\"]/ul/li/a" |> parseLinksValues |> List.map (trimBaseUrl baseUrl)
    | None -> []

let getSeasonEpisodes baseUrl url =
    let html = downloadHtml (baseUrl + url)
    match html with
    | Some(html) -> parseLinks html "//div[@id=\"content_box\"]/table/tbody/tr/th/div/a[1]" |> parseLinksValues |> List.map (trimBaseUrl baseUrl)
    | _ -> []

[<EntryPoint>]
let main argv = 
    let baseUrl = "http://project-free-tv.li"

    getSearch baseUrl "archer"
    |> List.map (fun resource -> (resource, classifyResource resource))
    |> List.iter (fun (url, resourceType) -> printfn "%s %s" url (if resourceType.IsSome then resourceType.Value.ToString() else "invalid"))
    |> ignore

    getShowSeasons baseUrl "/free/archer/"
    |> List.iter (printfn "%s")
    |> ignore

    getSeasonEpisodes baseUrl "/free/archer/archer-season-5/"
    |> List.iter (printfn "%s")
    |> ignore


    // /free/archer/archer-season-8/

    //let links = getEpisode 1 1

    //(Option.get links)
    //|> List.groupBy (fun link -> link.Provider)
    //|> List.iter (fun (key,links) -> printfn "%s" key; List.iter (fun link -> printfn "\t%s" link.Url) links)
    //|> ignore

    0 // return an integer exit code
