'use strict';

angular.module('App', [])
  .controller('AppController', function ($scope, $http, $interval) {
    var ws = new WebSocket('ws://localhost:8080/api/events');

    $interval(function () {
      ws.send('ping');
    }, 5000);
    $scope.simulation = false;
    $scope.settings = {
      minRegistrationsPause: 0,
      maxRegistrationsPause: 2,
      minWarehouseStay: 1,
      maxWarehouseStay: 5,
      registrationNumberLength: 7,
      lettersInRegistrationNumber: true,
      digitsInRegistrationNumber: true,
      simulationTimeout: 60
    };

    var numbers = _.range(0, 10);
    var letters = _.map(_.range(65, 91), function(num){ return String.fromCharCode(num); });

    $scope.settings.readProbabilities = [];

    $scope.$watch('settings.digitsInRegistrationNumber', function (value) {
      if(value) {
        $scope.settings.readProbabilities = _.union(_.map(numbers, function (char){
          return {
            correctCharacter: char,
            mistakes: []
          }
        }), $scope.settings.readProbabilities);
      } else {
        $scope.settings.readProbabilities = _.filter($scope.settings.readProbabilities, function(rp) {
          return !_.contains(numbers, rp.correctCharacter);
        });
      }
    });

    $scope.$watch('settings.lettersInRegistrationNumber', function (value) {
      if(value) {
        $scope.settings.readProbabilities = _.union($scope.settings.readProbabilities, _.map(letters, function (char){
          return {
            correctCharacter: char,
            mistakes: []
          }
        }));
      } else {
        $scope.settings.readProbabilities = _.filter($scope.settings.readProbabilities, function(rp) {
          return !_.contains(letters, rp.correctCharacter);
        });
      }
    });

    $scope.addMistake = function (character) {
      character.mistakes.push({
        mistake: "",
        probability: 0.0
      })
    };

    $scope.removeMistake = function (character, mistake) {
      character.mistakes = _.filter(character.mistakes, function (m) {
        return m.mistake != mistake.mistake;
      });
    };

    $scope.startSimulation = function () {
      var correctReadProbabilities = _.map($scope.settings.readProbabilities, function (rp) {
        return {
          correctCharacter: rp.correctCharacter + '',
          mistakes: mistakesObject(rp.mistakes)
        }
      });
      var objToSend = _.cloneDeep($scope.settings);
      objToSend.readProbabilities = correctReadProbabilities;
      $http.post('http://localhost:8080/api/start', objToSend);
      $scope.simulation = true;
    };

    function mistakesObject (mistakes) {
      var obj = {};
      _.forEach(mistakes, function (mistake) {
        obj[mistake.mistake + ''] = mistake.probability;
      });
      return obj;
    }

    $scope.data = {
      mistakenlyLetOut: 0,
      denied: 0,
      successful: 0,
      outStatus: "none"
    };

    ws.onmessage = function (data) {
      var eventData = JSON.parse(data.data);
      $scope.$apply(function () {
        if(eventData.in) {
          $scope.data.in = eventData.in;
        } else if (eventData.out) {
          $scope.data.out = eventData.out;
          $scope.data.outStatus = eventData.status;
        } else {
          $scope.data.warehouse = eventData.containers;
          $scope.data.mistakenlyLetOut = eventData.mistakenlyLetOut;
          $scope.data.denied = eventData.denied;
          $scope.data.successful = eventData.successful;
        }
      });
    };

  });