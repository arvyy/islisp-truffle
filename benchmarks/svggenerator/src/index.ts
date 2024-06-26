import { JSDOM } from 'jsdom';
import * as d3 from 'd3';
import * as d3_chromatic from 'd3-scale-chromatic';
import * as fs from 'node:fs';

const PADDING = 50;
const WIDTH = 400;
const HEIGHT = 400;
const BG_COLOR = '#151515';
const AXIS_COLOR = '#aaaaaa';

const GRADIENT_START = '#59d517';
const GRADIENT_END = '#17675c';

interface BenchmarkData {
    results: {
        command: string;
        mean: number;
    }[];
}

const args = process.argv.slice(2);
if (args.length != 2) {
    throw 'Must provide input json path and output svg path parameters';
}
const data: BenchmarkData = JSON.parse(fs.readFileSync(args[0]).toString());

const jsdom = new JSDOM(`
<!type HTML>
<html>
    <body>
        <svg xmlns="http://www.w3.org/2000/svg">
        </svg>
    </body>
</html>`);

const doc = jsdom.window.document;

const svg = d3.select(doc)
    .select('svg')
    .attr('width', WIDTH)
    .attr('height', HEIGHT)
    .classed('benchmark-chart', true);

svg.append('rect')
    .attr('width', WIDTH)
    .attr('height', HEIGHT)
    .attr('fill', BG_COLOR)

const categoryScale = d3.scaleBand()
    .paddingInner(0.1)
    .paddingOuter(0.1)
    .domain(data.results.map(r => r.command))
    .range([PADDING, WIDTH - PADDING]);
svg.append('g')
    .classed('x-axis', true)
    .attr('transform', `translate(0, ${HEIGHT - PADDING})`)
    .call(d3.axisBottom(categoryScale));

const colorScale = d3.scaleSequential()
    .domain([PADDING, WIDTH - PADDING])
    .interpolator(d3.interpolateRgb(GRADIENT_START, GRADIENT_END))

const valueScale = d3.scaleLinear()
    .domain([0, d3.max(data.results.map(r => r.mean))])
    .range([HEIGHT - PADDING, PADDING]);
svg.append('g')
    .classed('y-axis', true)
    .attr('transform', `translate(${PADDING}, 0)`)
    .call(d3.axisLeft(valueScale));

svg.select('g.x-axis')
    .selectAll('line')
    .attr('stroke', AXIS_COLOR);
svg.select('g.x-axis')
    .selectAll('path')
    .attr('stroke', AXIS_COLOR);
svg.selectAll('g.x-axis')
    .selectAll('text')
    .attr('fill', AXIS_COLOR);
svg.select('g.y-axis')
    .selectAll('line')
    .attr('stroke', AXIS_COLOR);
svg.select('g.y-axis')
    .selectAll('path')
    .attr('stroke', AXIS_COLOR);
svg.select('g.y-axis')
    .selectAll('text')
    .attr('fill', AXIS_COLOR);

const barGroup = svg.append('g')
    .selectAll('g.data-bar')
    .data(data.results)
    .enter()
    .append('g')
    .classed('data-bar', true)
    .attr('text-anchor', 'middle');

barGroup.append('rect')
    .attr('x', d => categoryScale(d.command))
    .attr('y', d => valueScale(d.mean))
    .attr('width', categoryScale.bandwidth())
    .attr('height', d => valueScale(0) - valueScale(d.mean))
    .attr('rx', 2)
    .attr('ry', 2)
    .attr('fill', d => colorScale(categoryScale(d.command)));
barGroup.append('text')
    .text(d => d3.format('.2')(d.mean))
    .attr('y', d => valueScale(d.mean) - 20)
    .attr('x', d => categoryScale(d.command) + categoryScale.bandwidth() / 2)
    .attr('fill', d => colorScale(categoryScale(d.command)));

fs.writeFileSync(args[1], doc.getElementsByTagName('svg')[0].outerHTML);
