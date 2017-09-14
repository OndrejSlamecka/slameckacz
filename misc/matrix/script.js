/**
 * Author: Ondrej Slamecka, www.slamecka.cz
 */

nColumns = 50;
nRows = 30;
nCodeHeight = 15;
nSpeed = 25;

nRColor = 63;
nGColor = 186;
nBColor = 66;


Matrix = new function()
{
    this.getColorShade = function(nLevel)
    {
        return 'color: rgba(' + Math.floor(nRColor * nLevel) + ',' + Math.floor(nGColor * nLevel) + ','  + Math.floor(nBColor * nLevel) + ',1)';
    }
}

function getRandom(min,max)
{
    return Math.floor(Math.random() * (max - min + 1)) + min;
}


function continueCode(y,x,expiration)
{
    // One expiring
    nFading = nCodeHeight-expiration+1;
    for(var i=0;i<=nFading;i++)
    {
        var ypos = (y-i);

        if (ypos < 0)
            ypos = ypos + nRows+1;

        if( ypos > nRows )
            ypos = ypos - nRows;

        if( aMatrix[ypos] ==undefined )
            console.log(i);
        var td = aMatrix[ypos][x];
        td.setAttribute( 'style', Matrix.getColorShade( (1-((i+1)/15)) ) );
    }

    // Creating new
    y;
    if( y > nRows ) // Y from 0, nRow from 1
        y = y - nRows-1;
    var td = aMatrix[y][x];
    td.innerHTML = String.fromCharCode( getRandom(96,122) );

    // Deacrease values and continue
    if( expiration > 0 )
        setTimeout(function(){ continueCode(y+1,x,expiration-1) }, nSpeed);
    else
        column();

}

function column()
{
    // select random cell at least nCodeHeight rows above bottom
    var y = getRandom(0,nRows);
    var x = getRandom(0,nColumns);

    var td = aMatrix[y][x];

    // write a char to the cell
    td.innerHTML = String.fromCharCode( getRandom(96,122) );

    //
    setTimeout(function(){ continueCode(y,x,nCodeHeight) }, nSpeed);

}

/* MAIN */
window.onload = function()
{
    oMatrix = document.getElementById('matrix');
    aMatrix = [];

    for(var i=0;i<=nRows;i++)
    {
        var tr = document.createElement('tr');
        oMatrix.appendChild(tr);
        tr.setAttribute('id','row'+i);

        aMatrix[i] = [];

        for(var j=0;j<=nColumns;j++)
        {
            var td = document.createElement('td');
            tr.appendChild(td);
            td.setAttribute('id','row'+i+'-col'+j);

			var div = document.createElement('div');
			td.appendChild(div);

            aMatrix[i][j] = div;
        }
    }

    for(var i=0;i<2;i++)
        column();
}
