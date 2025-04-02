"use strict";

/**
 * Start state of the game.
 */

const field = document.getElementById("field");
const stepButton = document.getElementById("step");
const inputHeight = document.getElementById("width");
const inputWidth = document.getElementById("height");
const createButton = document.getElementById("create");
const simulationButton = document.getElementById("simulation");

let width = 0;
let height = 0;
let life = null;
const checkboxes = [];
stepButton.disabled = true;
simulationButton.disabled = true;

/**
 * Create a field with checkboxes in random state.
 */

function createField() {
  width = inputWidth.value || 2;
  height = inputHeight.value || 2;

  if (isNaN(Number(height)) || isNaN(Number(width))) return;
  if (Number(width) > 42 || Number(width) > 42) return;

  for (let i = 0; i < height; i++) {
    const row = [];
    for (let i = 0; i < width; i++) {
      const checkbox = document.createElement("input");
      checkbox.type = "checkbox";
      if (Math.random() < 0.5) checkbox.checked = true;
      row.push(checkbox);
      field.appendChild(checkbox);
    }

    checkboxes.push(row);
    field.appendChild(document.createElement("br"));
  }

  createButton.disabled = true;
  inputHeight.disabled = true;
  inputWidth.disabled = true;
  stepButton.disabled = false;
  simulationButton.disabled = false;
}

/**
 * Update field following game rules every 200ms.
 */

function runSimulation() {
  const MILLISECONDS = 200;

  if (!life) {
    simulationButton.textContent = "stop";
    life = setInterval(createNewGeneration, MILLISECONDS);
  } else {
    simulationButton.textContent = "start";
    clearInterval(life);
    life = null;
  }
}

/**
 * Create new generation of the game following the rules:
 * 1. Any live cell with fewer than two live neighbors dies, as if by underpopulation.
 * 2. Any live cell with two or three live neighbors lives on to the next generation.
 * 3. Any live cell with more than three live neighbors dies, as if by overpopulation.
 */

function createNewGeneration() {
  const copyState = checkboxes
    .slice()
    .map((item) => item.map((item) => item.checked));
  for (let i = 0; i < height; i++) {
    for (let j = 0; j < width; j++) {
      const neighbors = countAliveNeighbors(i, j, copyState);
      if (copyState[i][j] && (neighbors > 3 || neighbors < 2))
        checkboxes[i][j].checked = false;
      if (!copyState[i][j] && neighbors === 3) checkboxes[i][j].checked = true;
    }
  }
}

/**
 * Count the number of alive neighbors. Ignore the cell 
 * itself and outside the field.
 */

function countAliveNeighbors(row, column, matrix) {
  let numberOfLiveNeighbors = 0;
  for (let i = row - 1; i < row + 2; i++) {
    for (let j = column - 1; j < column + 2; j++) {
      if (!matrix[i] || !matrix[i][j]) continue;
      if (i === row && j === column) continue;
      if (matrix[i][j]) numberOfLiveNeighbors++;
    }
  }
  return numberOfLiveNeighbors;
}

createButton.addEventListener("click", createField);
stepButton.addEventListener("click", createNewGeneration);
simulationButton.addEventListener("click", runSimulation);
