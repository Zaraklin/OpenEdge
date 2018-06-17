
/*------------------------------------------------------------------------
    File        : restConsumer.p
    Purpose     : 

    Syntax      :

    Description : Exemplo de codigo para consumir WebServices Rest

    Author(s)   : Rafael
    Created     : Tue Jun 12 14:18:32 BRT 2018
    Notes       :
  ----------------------------------------------------------------------*/

block-level on error undo, throw.

/** Carrega bibliotecas necessarias **/
using OpenEdge.Net.HTTP.IHttpClientLibrary.
using OpenEdge.Net.HTTP.ConfigBuilder.
using OpenEdge.Net.HTTP.ClientBuilder.
using OpenEdge.Net.HTTP.Credentials.
using OpenEdge.Net.HTTP.IHttpClient.
using OpenEdge.Net.HTTP.IHttpRequest.
using OpenEdge.Net.HTTP.RequestBuilder.
using OpenEdge.Net.URI.
using OpenEdge.Net.HTTP.IHttpResponse.

using Progress.Json.ObjectModel.JsonObject.
using Progress.Json.ObjectModel.JsonArray.

def var oClient        as IHttpClient        no-undo.
def var oUri           as URI                no-undo.
def var oReq           as IHttpRequest       no-undo.
def var oResp          as IHttpResponse      no-undo.
def var oCreds         as Credentials        no-undo.
        
def var oJsonRespObj   as JsonObject         no-undo.        
def var oJsonRespArray as JsonArray          no-undo.
def var oJsonObj       as JsonObject         no-undo.
def var oJsonArray     as JsonArray          no-undo.
        
def var i              as int                no-undo.

def temp-table tt-posts
    field usrId     as char format "x(12)"
    field id        as char format "x(12)" 
    field postTitle as char format "x(50)"
    field postBody  as char format "x(75)".

/* *************************** GET *************************** */
/*************************************************************************
    URL Destino: GET https://jsonplaceholder.typicode.com/posts
    
    Padrao de retorno esperado:
    {
        {
            "userId": 1,
            "id": 1,
            "title": "sunt aut facere repellat provident occaecati excepturi optio reprehenderit",
            "body": "quia et suscipit\nsuscipit recusandae consequuntur expedita et cum\nreprehenderit molestiae ut ut quas totam\nnostrum rerum est autem sunt rem eveniet architecto"
        },
        ...
    }
 *************************************************************************/

/* INI - requisicao web */
assign oClient   = ClientBuilder:Build():Client       
       oUri      = new URI("http", "jsonplaceholder.typicode.com") /* URI("metodo", "dominio", "porta") */
       oUri:Path = "/posts".                                       /* URI:Path: demais partes da URL destino */
       
/* Faz a requisicao utilizando GET */
oReq  = RequestBuilder:Get(oUri)
             :Request.
oResp = oClient:Execute(oReq).
/* FIM - requisicao web */

/* trata o retorno */
empty temp-table tt-posts.

/* valida o tipo de retorno, se for Json processa normalmente */
if type-of(oResp:Entity, JsonArray) then do:
    
    oJsonRespArray = cast(oResp:Entity, JsonArray).
    
    oJsonArray = oJsonRespArray.        
	
	/* Mostra o total de registros retornados */
    message oJsonArray:length
        view-as alert-box.
    
	/* Converte de Json para Registros na temp-table */
	Loop1:
    do i=1 to oJsonArray:length on error undo, next:
        oJsonObj = oJsonArray:GetJsonObject(i).
        
        create tt-posts.
        assign tt-posts.usrId     = string(oJsonObj:GetInt64("userId"))
               tt-posts.id        = string(oJsonObj:GetInt64("id"))
               tt-posts.postTitle = oJsonObj:GetCharacter("title")
               tt-posts.postBody  = oJsonObj:GetCharacter("body").
    end.    
end.

/** Abaixo regras de negocio e tratamentos necessarios **/
for each tt-posts no-lock:
    disp tt-posts.usrId
         tt-posts.id
         tt-posts.postTitle skip
         tt-posts.postBody
         with side-labels width 100 title "Retorno GET".    
end.

/* *************************** POST *************************** */
/*************************************************************************
    URL Destino: POST https://jsonplaceholder.typicode.com/posts
    
    Padrao de envio:
    {
        title: 'foo',
        body: 'bar',
        userId: 1
    }
    
    Padrao de retorno esperado:
    {
        id: 101,
        title: 'foo',
        body: 'bar',
        userId: 1
    }
 *************************************************************************/

/* INI - requisicao web */
assign oClient   = ClientBuilder:Build():Client       
       oUri      = new URI("http", "jsonplaceholder.typicode.com") /* URI("metodo", "dominio", "porta") */
       oUri:Path = "/posts".                                       /* URI:Path: demais partes da URL destino */

/* Cria objeto Json e popula ele */
oJsonObj = new JsonObject().
oJsonObj:Add("title","Teste").
oJsonObj:Add("body","Teste Teste Teste").
oJsonObj:Add("userId",95).

/* Faz a requisicao utilizando POST */
oReq  = RequestBuilder:Post(oUri, oJsonObj)
             :Request.
oResp = oClient:Execute(oReq).
/* FIM - requisicao web */

/* trata o retorno */
empty temp-table tt-posts.

/* valida o tipo de retorno, se for Json processa normalmente */
if type-of(oResp:Entity, JsonObject) then do:
    
    oJsonRespObj = cast(oResp:Entity, JsonObject).
        
    create tt-posts.
    assign tt-posts.usrId     = string(oJsonRespObj:GetInt64("userId"))
           tt-posts.id        = string(oJsonRespObj:GetInt64("id"))
           tt-posts.postTitle = oJsonRespObj:GetCharacter("title")
           tt-posts.postBody  = oJsonRespObj:GetCharacter("body").        
end.

/** Abaixo regras de negocio e tratamentos necessarios **/
for each tt-posts no-lock:
    disp tt-posts.usrId
         tt-posts.id
         tt-posts.postTitle skip
         tt-posts.postBody
         with side-labels width 100 title "Retorno POST".    
end.


/* *************************** FIM *************************** */
message "FIM"
    view-as alert-box.
