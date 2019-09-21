package za.ac.sun.cs.coastal.observers;

import java.io.BufferedOutputStream;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Map.Entry;

import org.apache.commons.configuration2.ImmutableConfiguration;
import org.apache.logging.log4j.Logger;

import redis.clients.jedis.Jedis;
import za.ac.sun.cs.coastal.COASTAL;
import za.ac.sun.cs.coastal.diver.SymbolicState;
import za.ac.sun.cs.coastal.messages.Broker;
import za.ac.sun.cs.coastal.messages.Tuple;
import za.ac.sun.cs.coastal.solver.Expression;
import za.ac.sun.cs.coastal.solver.IntegerVariable;
import za.ac.sun.cs.coastal.solver.Operation;
import za.ac.sun.cs.coastal.solver.Operation.Operator;
import za.ac.sun.cs.coastal.solver.Visitor;
import za.ac.sun.cs.coastal.solver.VisitorException;

public class PCReporterFactory implements ObserverFactory {

    public PCReporterFactory(COASTAL coastal, ImmutableConfiguration options) {
    }

    @Override
    public int getFrequencyflags() {
        return ObserverFactory.ONCE_PER_TASK;
    }

    @Override
    public ObserverManager createManager(COASTAL coastal) {
        return new PCReporterManager(coastal);
    }

    @Override
    public Observer createObserver(COASTAL coastal, ObserverManager manager) {
        return new PCReporterObserver(coastal, manager);
    }

    // ======================================================================
    //
    // MANAGER FOR PATH CONDITION REPORTING
    //
    // ======================================================================

    private static class ConstraintCollectorVisitor extends Visitor {
        public HashSet<Operation> constraints = new HashSet<>();

        public void postVisit(Operation op) {
            if (op.getOperand(0) instanceof IntegerVariable) {
                if (op.getOperand(0).toString().contains("A")) {
                    constraints.add(op);
                }
            }
        }


    }

    private static class PCReporterManager implements ObserverManager {

        private final Broker broker;

        private final Logger log;

        private final Map<Boolean, HashSet<Expression>> conditions = new HashMap<>();

        PCReporterManager(COASTAL coastal) {
            broker = coastal.getBroker();
            log = coastal.getLog();
            broker.subscribe("coastal-stop", this::report);
        }

        /**
         * Filter out the constraints in a given path condition relevant to the input array "A"
         *
         * @param pathCondition
         * @return
         */
        private HashSet<Operation> extractConstraints(Expression pathCondition) {
            try {
                ConstraintCollectorVisitor collector = new ConstraintCollectorVisitor();
                pathCondition.accept(collector);
                return collector.constraints;
            } catch (VisitorException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
            return null;
        }

        /**
         * Transform a set of constraint expressions to a map of character position to upper and lower bounds.
         *
         * @param constraints
         * @return [lower bound, upper bound)
         */
        private HashMap<Integer, Integer[]> processConstraints(HashSet<Operation> constraints) {
            HashMap<Integer, Integer[]> bounds = new HashMap<>();

            constraints.forEach(expr -> {
                Integer charPosition = Integer.parseInt(expr.getOperand(0).toString().substring(2));
                Integer bound = Integer.parseInt(expr.getOperand(1).toString());

                Integer[] currentBounds = bounds.get(charPosition);

                if (currentBounds == null) {
                    currentBounds = new Integer[2];
                }

                Operator op = expr.getOperator();

                if (op == Operator.GE || op == Operator.GT) {
                    if (currentBounds[0] == null || currentBounds[0] < bound) {
                        currentBounds[0] = bound;
                    }
                } else if (op == Operator.LT || op == Operator.LE) {
                    if (currentBounds[1] == null || currentBounds[1] > bound) {
                        currentBounds[1] = bound;
                    }
                }
                bounds.put(charPosition, currentBounds);

            });

            return bounds;
        }

        public synchronized void update(Boolean accepted, Expression pathCondition) {

            HashSet<Expression> pathConditions = conditions.get(accepted);

            // Extract constraints on the input array
            HashSet<Operation> constraints = extractConstraints(pathCondition);

            // Transform these constraints in a format symbolic automata can understand
            HashMap<Integer, Integer[]> processedConstraints = processConstraints(constraints);

            // Construct the redis formatted output
            StringBuilder redisResponse = new StringBuilder();
            redisResponse.append("[");

            processedConstraints.forEach((pos, bounds) -> {
                redisResponse.append("[").append(bounds[0]).append(" ").append(bounds[1]).append("]");
            });

            redisResponse.append("]");

            // Write the respone to Redis
            Jedis jedis = new Jedis("redis");
            jedis.set("refined", redisResponse.toString());
        }

        public void report(Object object) {
            broker.publish("path-condition-report", null);

            if (conditions.size() > 0) {
                for (Entry<Boolean, HashSet<Expression>> entry : conditions.entrySet()) {
//					entry.getValue().forEach(cond -> broker.publish("report", new Tuple("PCReporter.pc[" + entry.getKey() + "]" + cond.toString())));
                }
            }

        }

        @Override
        public String getName() {
            return null;
        }

        @Override
        public String[] getPropertyNames() {
            return null;
        }

        @Override
        public Object[] getPropertyValues() {
            return null;
        }

    }

    // ======================================================================
    //
    // OBSERVER FOR MARKER COVERAGE
    //
    // ======================================================================

    private static class PCReporterObserver implements Observer {

        private final Logger log;

        private final PCReporterManager manager;

        private final Broker broker;

        private boolean accepted = false;

        private final Jedis jedis;

        PCReporterObserver(COASTAL coastal, ObserverManager manager) {
            log = coastal.getLog();
            this.manager = (PCReporterManager) manager;
            this.broker = coastal.getBroker();
            broker.subscribeThread("mark", this::mark);
            broker.subscribeThread("dive-end", this::diveEnd);
            this.jedis = new Jedis("redis");
        }

        public void diveEnd(Object object) {
            Tuple result = (Tuple) object;
            SymbolicState state = (SymbolicState) result.get(1);

            log.info(state.getSegmentedPathCondition().getPathCondition());

            manager.update(accepted, state.getSegmentedPathCondition().getPathCondition());
        }

        public void mark(Object object) {
            /* 1 represents acceptance, 0 represents failure */
            accepted = (((Integer) object) == 1) ? true : false;
        }

    }

}
