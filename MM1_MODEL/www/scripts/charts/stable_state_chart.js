/*
 * Function that allows me to validate that the information has been entered,
 * in case it is, it will be sent to the server.
 */
const validate_information = () => {
    /* I get the values from the inputs. */
    let lambda, mean, n;
    lambda = document.getElementById('lambda').value;
    lambda_result = parseFloat(lambda)
    mean = document.getElementById('mean').value;
    mean_result = parseFloat(mean)
    n = parseInt(document.getElementById('n').value);
    /* Validation */
    if (lambda.length > 0 && mean.length > 0 && n >= 0 ) {
        /* I save everything in a JSON that i will send to the server. */
        if(lambda<mean){
            let json = {
                lambda: lambda_result,
                mean: mean_result,
                n: n,
            }
        /* I send the information to Shiny. */
        Shiny.onInputChange("click", json);
        }
        else{
            alert("lamba should be smaller than mean")
        }
    } else {
        /* I send an alert that there are missing fields to fill out. */
        alert("Fill all the fields to continue.");
    }
}

/*
 * Function that allows me to generate the graph of the
 * probabilities of "n" users in the system.
 */
const generate_stable_state_chart = (info) => {
    /*  Generation of the chart. */
    let ctx = document.getElementById('stable_state_chart').getContext('2d');
    let stable_state_chart = new Chart(ctx, {
        type: 'bar',
        data: {
            labels: ['p0', 'p1', 'p2'],
            datasets: [{
                label: 'The probabilities (p0, p1, ..., pk) are:',
                data: [info[0][0], info[0][1], info[0][2]],
                backgroundColor: [
                    'rgba(255, 99, 132, 0.2)',
                    'rgba(54, 162, 235, 0.2)',
                    'rgba(255, 206, 86, 0.2)'
                ],
                borderColor: [
                    'rgba(255, 99, 132, 1)',
                    'rgba(54, 162, 235, 1)',
                    'rgba(255, 206, 86, 1)'
                ],
                borderWidth: 1
            }]
        },
        options: {
            scales: {
                yAxes: [{
                    ticks: {
                        beginAtZero: true
                    }
                }]
            }
        }
    });
    /* Generation of the statistical report. */
    /* I show the hidden paragraphs and then i print the information. */
    let p_length = document.getElementsByTagName("p").length;
    for (let i = 0; i < p_length; i++) {
        document.getElementsByTagName("p")[i].style.visibility = "visible";
    }
    document.getElementById('traffic-intensity').innerHTML = info[1];
    document.getElementById('server-use').innerHTML = info[2];
    document.getElementById('mean-number-clients-system').innerHTML = info[3];
    document.getElementById('mean-number-clients-queue').innerHTML = info[4];
    document.getElementById('mean-number-clients-server').innerHTML = info[5];
    document.getElementById('mean-time-spend-system').innerHTML = info[6];
    document.getElementById('mean-time-spend-queue').innerHTML = info[7];
    document.getElementById('mean-time-spend-server').innerHTML = info[8];
    document.getElementById('mean-time-spend-queue-where-queue').innerHTML = info[9];
    document.getElementById('throughput').innerHTML = info[10];
    document.getElementById('lazy').innerHTML = info[11];
    document.getElementById('sum-probs').innerHTML = info[12];
    document.getElementById('1-probs').innerHTML = info[13];
}